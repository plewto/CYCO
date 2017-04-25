;;;; cyco.sbcl-specific
;;;; Load only under SBCL

(in-package :cyco)

(defun create-cyco-core (filename)
  "Compiles CYCO as an executable. 
   Startup time is considerably faster.
   Currently only implemented for SBCL 

   **** CAUTION:  FILENAME IS OVERWRITTEN WITHOUT WARNING *****"
 
  (let ((itype (lisp-implementation-type)))
    (cond ((string= itype "SBCL")
	   (format t "Creating SBCL CYCO core as '~s'" filename)
	   (sb-ext::save-lisp-and-die filename :executable t))
	  (t (error (format nil "CREATE-CYCO-CORE not defined for ~s" itype))))))

