;;;; cyco.orch.keymaps
;;;;
;;;; A keynumber map is a function of form (lambda (n)) -> n' which maps one 
;;;; key number to another.   If the argument is :? the function should 
;;;; display useful help text and return an appropriate default value.
;;;; Any values which the map can not convert should be returned as is. 
;;;;

(in-package :cyco)

(defun default-keymap (name &key (no-warn t))
  #'(lambda (kn)
      (cond ((numberp kn)
	     (if (minusp kn)
		 +REST+
	       (logand kn 127)))
	    ((eq kn :?)
	     (format t "Default keymap for ~A~%" name)
	     +REST+)
	    (t
	     (if no-warn
		 kn
	       (cyco-warning (format nil "~A is not a valid ~A keynumber!"
				     kn name)))))))

(param default-keymap (default-keymap 'default :no-warn t))

(defun keymap (name alist &key (no-warn t))
  #'(lambda (kn)
      (let ((aval (assoc kn alist)))
	(cond (aval
	       (car (cdr aval)))
	      ((numberp kn)
	       (if (minusp kn)
		   +REST+
		 (car (cdr (cnth (truncate kn) alist)))))
	      ((eq kn :?)
	       (format t "~A keymap~%" name)
	       (dolist (a alist)
		 (format t "  ~12A -> ~A~%" (car a)(cdr a)))
	       +REST+)
	      (no-warn
	       kn)
	      (t
	       (cyco-warning (format nil "~A is not a valid ~A keynumber!"
				     kn name))
	       +REST+)))))

(defun circular-keymap (name keys &key (no-warn t))
  #'(lambda (kn)
      (cond ((numberp kn)
	     (if (minusp kn)
		 +REST+
	       (cnth kn keys)))
	    ((eq kn :?)
	     (format t "~A circular-keymap: ~A~%"
		     name keys))
	    (no-warn
	     kn)
	    (t
	     (cyco-warning (format nil "~A is not a valid ~A keynumber!"
				   kn name))
	     +REST+))))

(defun reduced-keymap (name lower upper &key (no-warn t))
  #'(lambda (kn)
      (cond ((numberp kn)
	     (if (minusp kn)
		 +REST+
	       (progn 
		 (while (< kn lower)(setf kn (+ kn 12)))
		 (while (> kn upper)(setf kn (- kn 12)))
		 kn)))
	    ((eq kn :?)
	     (format t "Reduced keymap for ~A:  [~A..~A]~%"
		     name lower upper)
	     +REST+)
	    (no-warn
	     kn)
	    (t
	     (cyco-warning (format nil "~A is not a valid ~A keynumber!"
				   kn name))
	     +REST+))))
	     

