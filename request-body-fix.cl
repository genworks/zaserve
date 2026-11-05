;;; request-body-fix.cl -- Allegro only: loaded by the #+allegro zaserve
;;; shim after (require :aserve), beside read-sock-line-fix.cl.  On CCL,
;;; SBCL and the other zacl hosts the same fix is in main.cl and
;;; publish.cl.
;;;
;;; Genworks, 2026-11-04.  To go upstream to Franz; drop this file once a
;;; fixed aserve ships.
;;;
;;; 1. THE BODY.  get-request-body reads a whole body into one string and
;;;    runs without the application asking: with-http-body reads the body
;;;    of any kept-alive reply, and request-query reads a form-encoded
;;;    POST.  The built-in wserver-max-content-length covers a declared
;;;    length only, and signals an error from a place that gives the
;;;    client no reply.  *max-request-body-bytes* covers chunked and
;;;    read-to-end bodies as well, and the request still gets its reply.
;;; 2. THE WORKER.  http-worker-thread handles error, and running out of
;;;    heap or stack is a storage-condition, so the worker went to the
;;;    debugger and did not come back.  That is fixed whatever the limit.
;;;
;;; Nothing here copies the built-in's code: the built-in's functions are
;;; wrapped, so this serves any aserve that has them.
;;;
;;; THE LIMIT IS OFF UNTIL SOMEONE SETS IT, unlike main.cl's.  A proxy
;;; built on this aserve forwards whatever get-request-body gives it, so a
;;; limit it did not choose would turn a long body into an empty one.

(in-package :net.aserve)

(defvar *max-request-body-bytes* nil
  "The longest body get-request-body holds in memory, in bytes; nil is no
limit.  A longer body, declared or found while reading, is not kept:
get-request-body answers \"\", and the connection closes after the reply.
get-multipart-sequence, get-request-body-incremental and
with-body-input-stream hold nothing and are not limited.")

(defvar *max-drained-body-bytes* (* 128 1024 1024)
  "How much of a refused body is read and dropped after the reply goes
out, since closing with bytes unread can reset the connection ahead of the
reply.  It stops at this many bytes or after five seconds.")

(defun body-over-limit-p (bytes)
  (and *max-request-body-bytes*
       (> bytes *max-request-body-bytes*)))

(defun refuse-request-body (req)
  ;; req's body is too long to hold: remember that, and close after the
  ;; reply.  compute-strategy sees to a reply not begun yet.
  (setf (getf (request-reply-plist req) 'body-refused) t)
  (setf (request-reply-strategy req)
    (remove :keep-alive (request-reply-strategy req) :test #'eq)))

(defun request-body-refused-p (req)
  ;; true when req's body was refused, or when it is over the limit by
  ;; its own declaration and nothing has claimed it.  A streaming reader
  ;; claims the body by setting request-request-body.
  (or (getf (request-reply-plist req) 'body-refused)
      (and (null (request-request-body req))
           (multiple-value-bind (length believe-it)
               (header-slot-value-integer req :content-length)
             (and believe-it (body-over-limit-p length))))))

(defun drain-refused-body (req)
  ;; the reply has gone out.  Close after this, reading and dropping a
  ;; bounded part of the body first: see *max-drained-body-bytes*.
  (logmess (format nil "refused a request body over ~d bytes: ~s"
                   *max-request-body-bytes* (request-raw-request req)))
  (setf (request-reply-strategy req)
    (remove :keep-alive (request-reply-strategy req) :test #'eq))
  (let ((sock (request-socket req))
        (scratch (make-array 65536 :element-type '(unsigned-byte 8)))
        (left *max-drained-body-bytes*)
        (deadline (+ (get-universal-time) 5)))
    (multiple-value-bind (length believe-it)
        (header-slot-value-integer req :content-length)
      (when believe-it (setq left (min left length))))
    (setf (stream-external-format sock) (find-external-format :octets))
    ;; with-timeout-local takes its time from the socket's read timeout,
    ;; and that bounds one read, not a client that trickles: hence the
    ;; deadline as well.
    (ignore-errors (socket:socket-control sock :read-timeout 5))
    (with-timeout-local (5 nil)
      (loop
        (when (or (<= left 0) (> (get-universal-time) deadline))
          (return))
        (let ((got (read-sequence scratch sock
                                  :end (min left (length scratch))
                                  :partial-fill t)))
          (when (<= got 0) (return))
          (decf left got))))))

(defun new-body ()
  (make-array 2048 :element-type 'character :adjustable t :fill-pointer 0))

(defun add-to-body (char ans)
  ;; true when ans is now over the limit.  It grows by doubling, but never
  ;; past one character over the limit, where the read stops: past that a
  ;; doubling would ask for twice the limit in one allocation.
  (let ((size (length ans)))
    (vector-push-extend
     char ans (max 1 (min size (- (1+ *max-request-body-bytes*) size)))))
  (body-over-limit-p (fill-pointer ans)))

(defun read-chunked-body-within-limit (req)
  ;; the built-in's incremental reader does the dechunking and the
  ;; trailers; it has no way to stop, so leaving is by return-from.
  (let ((ans (new-body)))
    (get-request-body-incremental
     req
     (lambda (buffer count)
       (dotimes (i count)
         (when (add-to-body (code-char (aref buffer i)) ans)
           (refuse-request-body req)
           (return-from read-chunked-body-within-limit ""))))
     :buffer (make-array 65536 :element-type '(unsigned-byte 8)))
    ans))

(defun read-body-to-end-within-limit (req)
  ;; not the incremental reader: past its first buffer, its read-to-end
  ;; branch calls the function without the buffer.
  (let* ((sock (request-socket req))
         (original-ef (stream-external-format sock))
         (ans (new-body)))
    (when (request-has-continue-expectation req)
      (send-100-continue req))
    (unwind-protect
         (progn
           (setf (stream-external-format sock) (find-external-format :octets))
           (with-timeout-local ((wserver-read-request-body-timeout *wserver*) nil)
             (loop
               (let ((ch (read-char sock nil :eof)))
                 (when (eq ch :eof) (return ans))
                 (when (add-to-body ch ans)
                   (refuse-request-body req)
                   (return ""))))))
      (setf (stream-external-format sock) original-ef))))

(excl:def-fwrapper limit-request-body (req)
  (multiple-value-bind (length believe-it)
      (header-slot-value-integer req :content-length)
    (cond ((null *max-request-body-bytes*) (excl:call-next-fwrapper))
          ((and believe-it (body-over-limit-p length))
           (refuse-request-body req)
           "")
          (believe-it (excl:call-next-fwrapper))
          ((equalp "chunked" (header-slot-value req :transfer-encoding))
           (read-chunked-body-within-limit req))
          ((or (keep-alive-specified req)
               (not (member (request-method req) '(:put :post :patch))))
           (excl:call-next-fwrapper))
          (t (read-body-to-end-within-limit req)))))

(excl:def-fwrapper keep-worker-through-storage-condition (sock)
  (declare (ignorable sock))
  (handler-case (excl:call-next-fwrapper)
    (storage-condition (cond)
      (logmess (format nil "worker gave up its request: ~a" cond))
      nil)))

(defmethod compute-strategy :around ((req http-request) (ent entity) format)
  ;; a body too long to hold is not read, so the connection cannot be
  ;; kept: its bytes are still in the way of the next request
  (declare (ignore format))
  (let ((strategy (call-next-method)))
    (if (request-body-refused-p req)
        (setf (request-reply-strategy req)
          (remove :keep-alive strategy :test #'eq))
        strategy)))

(defmethod handle-request :after ((req http-request))
  (when (request-body-refused-p req)
    (force-output-noblock (request-socket req))
    (drain-refused-body req)))

(excl:without-package-locks
  (excl:fwrap 'get-request-body-retrieve :limit-request-body 'limit-request-body)
  (excl:fwrap 'process-connection :keep-worker 'keep-worker-through-storage-condition))
