;;;; cyco.util.os
;;;;
;;;; Implements a subset of Python's os.path module. Currently only *nix
;;;; type systems are supported.

(in-package :cyco)

;; (param *os-path-separator* #\/)
;; (param *os-root* #\/)
;; (param *os-extension-separator* ".")

(param *os-homedir-alias* #\~)

(defun home-dir ()
  (let ((dir (->string (user-homedir-pathname))))
    (subseq dir 0 (1- (length dir)))))

(defun file-exists (fname)
  (probe-file fname))

(defun partition-path (fname)
  (split-string (namestring fname) *os-path-separator*))

(defun split-path (fname)
  (setf fname (namestring fname))
  (let ((pos (position *os-path-separator* fname 
		       :test #'string= :from-end t)))
    (if pos
	(list (subseq fname 0 pos)
	      (subseq fname (1+ pos)))
      (list fname nil))))

(defun split-extension (fname)
   (let ((pos (position *os-extension-separator* fname 
			:test #'string= :from-end t)))
     (if pos
	 (list (subseq fname 0 pos)
	       (subseq fname (1+ pos)))
       (list fname nil))))

(defun join-path-list (lst)
  (let ((delim (->string *os-path-separator*))
	(acc ""))
    (dolist (a lst)
      (setf acc (str+ acc (format nil "~A~A" (string-trim delim a) delim))))
    (setf acc (string-trim delim acc))
    (if (char= (char (->string (car lst)) 0) *os-path-separator*)
	(setf acc (str+ delim acc)))
    (if (char= (final (->string (final lst))) *os-path-separator*)
	(setf acc (str+ acc delim)))
    acc))

(defun join-path (&rest args)
  (join-path-list args))

(defun cwd ()
  (namestring *default-pathname-defaults*))

;; ISSUE: Portability?
(defun is-absolute-path (name)
  (and (char= (char (namestring name) 0) *os-root*) name))

(let ((previous-dir (cwd)))
  (flet ((swap-previous ()
			(let ((pd previous-dir))
			    (setf previous-dir *default-pathname-defaults*)
			    (setf *default-pathname-defaults* pd)))
	 (cd-parent-no-arg ()
			   (let ((parent (car (split-path (butfinal (cwd))))))
			     (setf previous-dir (cwd))
			     (setf *default-pathname-defaults*
				   (parse-namestring (str+ parent *os-path-separator*)))))
	 (cd-parent (arg)
		    (setf previous-dir (cwd))
		    (let* ((parent (car (split-path (butfinal (cwd)))))
			   (new-path (join-path parent (subseq arg 3))))
		      (if (not (char= (final new-path) *os-path-separator*))
			  (setf new-path (str+ new-path *os-path-separator*)))
		      (setf *default-pathname-defaults*
			    (parse-namestring new-path))))
	 (cd-cwd (arg)
		 (if (string= arg ".")
		     nil
		   (let ((new-path (str+ (cwd)(subseq arg 2))))
		     (setf previous-dir (cwd))
		     (if (not (char= (final new-path) *os-path-separator*))
			 (setf new-path (str+ new-path *os-path-separator*)))
		     (setf *default-pathname-defaults*
			   (parse-namestring new-path)))))
	 (cd-abs (arg)
		 (setf previous-dir (cwd))
		 (let ((new-path arg))
		   (if (not (char= (final new-path) *os-path-separator*))
		       (setf new-path (str+ new-path *os-path-separator*)))
		   (setf *default-pathname-defaults*
			 (parse-namestring new-path))))
	 (test-cwd ()
		   (if (not (file-exists (cwd)))
		       (cyco-warning (format nil "CWD '~A' does not exists!" (cwd))))))

    ;; cd special characters
    ;;   ~ -> home
    ;;   . -> current dir
    ;;  .. -> parent dir
    ;; nil -> switch to previous directory
    ;;
    (defun cd (&optional arg)
      (let ((sarg (->string arg)))
	(cond ((not arg)
	       (swap-previous))

	      ((string= sarg "~")
	       (swap-previous)
	       (setf *default-pathname-defaults* (parse-namestring (home-dir))))
	      
	      ((string= sarg "..")
	       (cd-parent-no-arg))

	      ((string-starts-with ".." arg)
	       (cd-parent arg))

	      ((string-starts-with "." arg)
	       (cd-cwd arg))

	      ((not (string-starts-with (->string *os-path-separator*) sarg))
	       (cd-cwd (str+ "." *os-path-separator* sarg)))
	
	      (t (cd-abs arg)))
	(test-cwd)
	(cwd)) )))

;; list files and directories on path or CWD
;;
(defun ls (&optional path)
  (let ((spath (format nil "~A*" (or path (cwd)))))
    (dolist (f (directory spath))
      (format t "~A~%" f))))

;; list all sub-directories on path or CWD
;;
(defun lsd (&optional path)
  (let ((spath (format nil "~A*" (or path (cwd)))))
    (dolist (f (directory spath))
      (if (char= (final (->string f)) *os-path-separator*)
	  (format t "~A~%" f)))))
    
