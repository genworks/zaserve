;;; read-sock-line-fix.cl -- Allegro only: loaded by the #+allegro zaserve
;;; shim right after (require :aserve), so every Allegro image that loads
;;; :zaserve (gwl does) runs this read-sock-line instead of the built-in
;;; one.  On CCL, SBCL and EGCL the same fix is in main.cl itself.
;;;
;;; Genworks, 2026-10-18.  Sent upstream to Franz (the built-in aserve
;;; 1.3.90 in Allegro CL 11.0, and franzinc/aserve master 1.3.91, both
;;; carry the defects); drop this file once a fixed aserve ships.
;;;
;;; 1. THE OVERRUN.  read-sock-line guards its content store (grow when
;;;    start >= max) but not the NUL terminator.  A line whose content is
;;;    exactly the buffer length (2048, doubling to 4096, 8192, ...) and
;;;    that ends in a bare LF writes the NUL one past the end of the
;;;    array; at (speed 3) with the buffer type declared nothing checks
;;;    it, the adjacent heap object is corrupted, and a later scavenge
;;;    dies with "System Error (gsgc) signal 11 in gc".  The terminator
;;;    now gets the same grow-if-needed guard.
;;; 2. THE UNBOUNDED LINE.  A line grows for as long as the client sends
;;;    bytes without a newline.  A line longer than
;;;    *max-request-line-length* (32768; nil = no limit) is now refused in
;;;    the reader -- buffer freed, nil returned, the eof path -- so aserve
;;;    answers 400 or drops the connection before anything parses.
;;;    *overlong-request-lines* counts refusals.
;;;
;;; Written in plain CL so the same body serves any aserve that has
;;; get-request-buffer / free-request-buffer.

(in-package :net.aserve)

(defvar *max-request-line-length* 32768
  "The longest request line or header line read-sock-line accepts, in
characters; a longer one drops the connection.  NIL is no limit.")

(defvar *overlong-request-lines* 0
  "How many lines read-sock-line has refused as over
*max-request-line-length* since this process started (approximate under
concurrency).")

(defun read-sock-line (sock buffer start chars-seen)
  (declare (optimize (speed 3))
           (type fixnum start)
           (type (simple-array character (*)) buffer))
  ;; read a line of data into the socket buffer, starting at start.
  ;; return buffer and index after last character in buffer; grow the
  ;; buffer when needed, both for content AND for the terminator.  If
  ;; problems occur free the passed-in buffer and return nil.
  (let ((max (length buffer))
        (prevch))
    (flet ((ensure-room ()
             (when (>= start max)
               (let ((new-buffer (get-request-buffer (* max 2))))
                 (declare (type (or null (simple-array character (*)))
                                new-buffer))
                 (cond
                   ((null new-buffer)
                    (free-request-buffer buffer)
                    (return-from read-sock-line nil))
                   (t (dotimes (i start)
                        (setf (schar new-buffer i) (schar buffer i)))
                      (setq max (length new-buffer))
                      (free-request-buffer buffer)
                      (setq buffer new-buffer)))))))
      (loop
        (let ((ch (read-char sock nil :eof)))
          (when (eq ch :eof)
            (debug-format :info "eof on socket")
            (free-request-buffer buffer)
            (return-from read-sock-line nil))
          (when (null (car chars-seen))
            (setf (car chars-seen) t))
          (cond
            ((eq ch #\linefeed)
             (when (eq prevch #\return)
               (decf start))                   ; toss out the CR
             (ensure-room)                      ; the terminator needs a slot too
             (setf (schar buffer start) #\null)
             (return-from read-sock-line (values buffer start)))
            (t
             (let ((cap *max-request-line-length*))
               (when (and cap (>= start cap))   ; refuse an overlong line early
                 (incf *overlong-request-lines*)
                 (free-request-buffer buffer)
                 (return-from read-sock-line nil)))
             (ensure-room)
             (setf (schar buffer start) ch)
             (incf start)))
          (setq prevch ch))))))
