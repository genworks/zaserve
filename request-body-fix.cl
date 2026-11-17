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
;;; 3. THE ALLOCATION (2026-11-16).  The built-in makes the whole string
;;;    from the declared length before a byte of body has come, so a client
;;;    that declares the limit and sends nothing holds it for the body-read
;;;    timeout.  A declared length over 64K is now read as it arrives: 64K
;;;    first, fourfold, then the whole length once an eighth of it has
;;;    come.  Also whatever the limit.
;;; 4. THE TOTAL (2026-11-16).  A limit bounds one request; many at once
;;;    still add up.  *max-body-bytes-in-flight* is a budget for all bodies
;;;    being read together, charged as their strings grow and given back
;;;    when each request ends.  A read that would pass it waits a few
;;;    seconds for room, then is turned away (marked :budget).  Off until
;;;    set, as the limit is.
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

(defun refuse-request-body (req &optional (why t))
  ;; req's body is not to be held: remember that, and why (t for a body
  ;; over the limit, :budget for one turned away because the bodies in
  ;; flight were at their budget), and close after the reply.
  ;; compute-strategy sees to a reply not begun yet.
  (setf (getf (request-reply-plist req) 'body-refused) why)
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
  (logmess (if (eq (getf (request-reply-plist req) 'body-refused) :budget)
               (format nil "turned away a request body, the bodies in flight at their budget of ~d: ~s"
                       *max-body-bytes-in-flight* (request-raw-request req))
               (format nil "refused a request body over ~d bytes: ~s"
                       *max-request-body-bytes* (request-raw-request req))))
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

(defvar *max-body-bytes-in-flight* nil
  "The most body, in characters, all requests together may hold while they
read: a backstop under *max-request-body-bytes*, which bounds one request and
not the total.  A read that would pass it waits up to *body-budget-wait*
seconds for room, then is turned away like a body over the limit, marked
:budget.  Charged as the string grows, so an idle connection costs it 64K.
Keep it above the limit, or a body at the limit is never admitted.  NIL, the
default here as for the limit, is no budget.")

(defvar *body-budget-wait* 5
  "Seconds a read waits for room in the budget before it is turned away.")

(defvar *body-bytes-in-flight* 0)

(defvar *body-budget-lock* (mp:make-process-lock :name "body budget"))

(defun reserve-body-bytes! (req n)
  ;; take N more characters for REQ's body from the budget, waiting up to
  ;; *body-budget-wait* seconds for room; true when taken
  (if (or (null *max-body-bytes-in-flight*) (<= n 0))
      t
      (let ((deadline (+ (get-internal-real-time)
                         (* *body-budget-wait* internal-time-units-per-second))))
        (loop
          (when (mp:with-process-lock (*body-budget-lock*)
                  (when (<= (+ *body-bytes-in-flight* n) *max-body-bytes-in-flight*)
                    (incf *body-bytes-in-flight* n)
                    (incf (getf (request-reply-plist req) 'body-reserved 0) n)
                    t))
            (return t))
          (when (> (get-internal-real-time) deadline) (return nil))
          (sleep 0.05)))))

(defun release-body-bytes! (req)
  ;; give back what REQ's body took from the budget; at the end of the
  ;; request, however it ends (handle-request :around below)
  (let ((n (getf (request-reply-plist req) 'body-reserved 0)))
    (when (> n 0)
      (mp:with-process-lock (*body-budget-lock*)
        (decf *body-bytes-in-flight* n))
      (setf (getf (request-reply-plist req) 'body-reserved) 0))))

(defun new-body ()
  (make-array 2048 :element-type 'character :adjustable t :fill-pointer 0))

(defun add-to-body (req char ans)
  ;; add CHAR to ANS: :limit when ANS is now over the limit, :budget when it
  ;; could not grow within the budget of bodies in flight, else nil.  It
  ;; grows by doubling, but never past one character over the limit, where
  ;; the read stops: past that a doubling would ask for twice the limit in
  ;; one allocation.
  (let* ((size (length ans))
         (extension (max 1 (min size (- (1+ *max-request-body-bytes*) size)))))
    (when (and (= (fill-pointer ans) (array-dimension ans 0))
               (not (reserve-body-bytes! req extension)))
      (return-from add-to-body :budget))
    (vector-push-extend char ans extension))
  (and (body-over-limit-p (fill-pointer ans)) :limit))

(defun read-chunked-body-within-limit (req)
  ;; the built-in's incremental reader does the dechunking and the
  ;; trailers; it has no way to stop, so leaving is by return-from.
  (let ((ans (new-body)))
    (get-request-body-incremental
     req
     (lambda (buffer count)
       (dotimes (i count)
         (let ((why (add-to-body req (code-char (aref buffer i)) ans)))
           (when why
             (refuse-request-body req (if (eq why :budget) :budget t))
             (return-from read-chunked-body-within-limit "")))))
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
                 (let ((why (add-to-body req ch ans)))
                   (when why
                     (refuse-request-body req (if (eq why :budget) :budget t))
                     (return "")))))))
      (setf (stream-external-format sock) original-ef))))

(defun read-declared-body-as-it-arrives (req length)
  ;; what the built-in's read of a declared length does -- the :octets
  ;; format, 100 Continue, a stray CRLF after the body eaten -- but the
  ;; string starts at 64K, grows fourfold as the bytes arrive, and goes to
  ;; the whole LENGTH once an eighth of it has come, so what is held follows
  ;; what the client has sent, not what it declared.  Few steps, because
  ;; Allegro keeps each step's string until a global collection.  Each step
  ;; is charged to the budget of bodies in flight; a step it cannot take
  ;; turns the body away and answers "".  nil if the body ends early or
  ;; the read times out, as the built-in's.  (2026-11-16.)
  (unless (reserve-body-bytes! req (min length 65536))
    (refuse-request-body req :budget)
    (return-from read-declared-body-as-it-arrives ""))
  (let* ((sock (request-socket req))
         (original-ef (stream-external-format sock)))
    (unwind-protect
         (progn
           (setf (stream-external-format sock) (find-external-format :octets))
           (when (and (member (request-method req) '(:put :post :patch))
                      (request-has-continue-expectation req))
             (send-100-continue req))
           (prog1 (with-timeout-local ((wserver-read-request-body-timeout *wserver*) nil)
                    (let ((ans (make-string (min length 65536)))
                          (got 0))
                      (loop
                        (when (>= got length) (return ans))
                        (when (>= got (length ans))
                          (let ((size (if (>= got (floor length 8))
                                          length
                                          (min length (* 4 (length ans))))))
                            (unless (reserve-body-bytes! req (- size (length ans)))
                              (refuse-request-body req :budget)
                              (return ""))
                            (let ((new (make-string size)))
                              (replace new ans)
                              (setq ans new))))
                        (let ((this (read-sequence ans sock :start got :end (length ans)
                                                            :partial-fill t)))
                          (when (<= this got) (return nil))
                          (setq got this)))))
             ;; not after a body turned away part way: the rest of it
             ;; is still there, for the drain
             (let ((ch (unless (getf (request-reply-plist req) 'body-refused)
                         (read-char-no-hang sock nil nil))))
               (cond ((eql ch #\return)
                      (let ((next (read-char-no-hang sock nil nil)))
                        (when (and next (not (eql next #\linefeed)))
                          (unread-char next sock))))
                     (ch (unread-char ch sock))))))
      (setf (stream-external-format sock) original-ef))))

(excl:def-fwrapper limit-request-body (req)
  (multiple-value-bind (length believe-it)
      (header-slot-value-integer req :content-length)
    (cond ((and believe-it (body-over-limit-p length))
           (refuse-request-body req)
           "")
          ;; a declared length is held as it arrives, limit or not; a
          ;; small one is left to the built-in
          ((and believe-it (> length 65536))
           (read-declared-body-as-it-arrives req length))
          ((or believe-it (null *max-request-body-bytes*))
           (excl:call-next-fwrapper))
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
      (logmess (or (ignore-errors (format nil "worker gave up its request: ~a" cond))
                   "worker gave up its request: a storage condition"))
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

(defmethod handle-request :around ((req http-request))
  ;; what this request's body took from the budget of bodies in flight
  ;; goes back when the request ends, however it ends
  (unwind-protect (call-next-method)
    (release-body-bytes! req)))

(defmethod handle-request :after ((req http-request))
  (when (request-body-refused-p req)
    (force-output-noblock (request-socket req))
    (drain-refused-body req)))

(excl:without-package-locks
  (excl:fwrap 'get-request-body-retrieve :limit-request-body 'limit-request-body)
  (excl:fwrap 'process-connection :keep-worker 'keep-worker-through-storage-condition))
