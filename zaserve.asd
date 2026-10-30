;;; ASD file contributed by james anderson <james.anderson@setf.de>
(in-package :cl-user)

(defvar *loadswitch* :compile-if-needed)
(defparameter *aserve-root* (directory-namestring *load-pathname*))

#+allegro
(defpackage #:acl-reader (:export #:cl-file))

#+allegro
(defclass acl-reader:cl-file (asdf:cl-source-file)
  ((type :initform "cl")))

#+allegro
(asdf:defsystem :zaserve
    :name "AllegroServe (portable)"    
  :author "John K. Foderaro"
  :version "1.3.65"
  :licence "LLGPL"
  :description "Trivial shim to load built-in aserve when running on Allegro CL" 
  ;; read-sock-line-fix: the built-in aserve's request-line reader with
  ;; the 2026-10-18 overrun fix and line cap (see that file); on the
  ;; other hosts the same fix is in main.cl.
  :serial t
  :components ((acl-reader:cl-file "require-original-aserve")
               (acl-reader:cl-file "read-sock-line-fix")))


#-allegro
(defun check-platform-compatibilty ()
  (unless (or (member :ccl *features*)
	      (member :sbcl *features*)
	      (member :egcl *features*)
	      (member :dotcl *features*)
	      (member :ecl *features*)
	      (member :abcl *features*))
    (error "

SORRY:
=====

This version of AllegroServe, which uses the `zacl' compatibility
layer (instead of this old acl-compat), is not currently supported on
~a. Please consider contributing, or requesting, a port of zacl for ~a.


"
	   (lisp-implementation-type) (lisp-implementation-type))))

#-(or zacl allegro)
;; zacl's own definition of this package, word for word (zacl
;; package.lisp): ABCL's DEFPACKAGE does not add a :USE or an :EXPORT
;; to a package that exists, so a shorter one made here first would
;; stand when zacl then loads.
(defpackage :zacl-reader
  (:use #:cl)
  (:export #:*allegro-rewriting-readtable* #:cl-file))

#-allegro
(asdf:defsystem
 :zaserve
 :depends-on (:zacl)
 :defsystem-depends-on (:zacl)
 :version "1.3.65"
 :name "AllegroServe"
 :author "John K. Foderaro"
 :licence "LLGPL"
 :description "AllegroServe, the HTTP/1.1 server, client, HTML generator and
 proxy from Franz, running on Clozure CL, SBCL, ECL, Evergreen Common Lisp
 and dotcl through the zacl compatibility layer."
 :components
 ;; this list is in load.cl as well... keep in sync
 ((:module "htmlgen" :components ((zacl-reader:cl-file "htmlgen")
                                  (:static-file "ChangeLog")))
  (zacl-reader:cl-file "packages")
  (zacl-reader:cl-file "macs")
  (zacl-reader:cl-file "queue")
  (zacl-reader:cl-file "main")
  (zacl-reader:cl-file "headers")
  (zacl-reader:cl-file "parse")
  (zacl-reader:cl-file "decode")
  (zacl-reader:cl-file "publish")
  (zacl-reader:cl-file "authorize")
  (zacl-reader:cl-file "log" )
  (zacl-reader:cl-file "cache")
  (zacl-reader:cl-file "client")
  (zacl-reader:cl-file "proxy")
  (zacl-reader:cl-file "cgi")
  (zacl-reader:cl-file "chunker")
  #+include-playback (zacl-reader:cl-file "playback")

  (:static-file "README.md")
  (:static-file "ChangeLog")
  (:static-file "license-lgpl.txt")
  (:static-file "LICENSE")
  (:static-file "load"))
 :perform (asdf:load-op :before (op zaserve)
			(check-platform-compatibilty))
 :serial t)



