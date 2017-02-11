;;;; cyco.orch.keymaps
;;;;
;;;; A keynumber map is a function of form (lambda (n)) -> n' which maps one 
;;;; key number to another.   If the argument is :? the function should 
;;;; display useful help text and return an appropriate default value.
;;;; Any values which the map can not convert should be returned as is. 
;;;;

(in-package :cyco)

(defun default-keymap (n)
  "The default keymap maps a keynumber to itself."
  (cond ((eq n :?)
	 (progn 
	   (format t "default keymap [0 : 127]~%")
	   60))
	((keynumber-p n)(keynumber n))
	(t ;(warning (format nil "~A is an Illegal keynumber" n))
	   60)))

(defmacro defkeymap (name alist)
  "Creates keymap function with symbolic mapping useful for percussion 
   instruments.

   name - the instrument name (used for help text)
   alist - An assoc list of the form ((symbol1 . (keynumber remarks))
                                     (symbol2 . (keynumber remarks))
                                      .............................)
   The remarks are optional but they appear in the help text."
  `(let ((acc '()))
     (dolist (a ,alist)
       (push (cons (car a)(->list (cdr a))) acc))
     (setf acc (reverse acc))
     (param ,name nil)
     (setf (symbol-function ',name)
	   #'(lambda (kn)
	       (let ((aval (assoc kn acc)))
		 (cond
		  ((eq kn :?)
		   (format t "~A keymap~%" (->string ',name))
		   (dolist (a acc)
		     (format t "    ~12A -> ~A~%" (car a)(cdr a)))
		   (car (cdr (car acc))))
		  (aval (car (cdr aval)))
		  ((keynumber-p kn)
		   (car (cdr (cnth (keynumber kn) acc))))
		  (t (let ((frmt "~A is invalid ~A keynumber"))
		       (warning (format nil frmt kn (->string ',name)))
		       (car (cdr (car acc)))))))))))
			  
(defmacro circular-keymap (name keys)
  "Creates keynumber map with circular assignments.   Any out of bound
   argument is coerced to a proper value using REM.
    
   name - instrument name (only used for help text)
   keys - list of key numbers."
  `(let ((count (length ,keys)))
     (param ,name nil)
     (setf (symbol-function ',name)
	   #'(lambda (kn)
	       (if (eq kn :?)
		   (progn 
		     (format t "~A circular keymap~%" (->string ',name))
		     (format t "    [  0] -> ~A~%" (car ,keys))
		     (format t "    [~3D] -> ~A~%" count (car (reverse ,keys)))
		     (car ,keys))
		 (cnth (keynumber kn) ,keys))))))
		     
(defmacro reduced-keymap (name lower upper)
  "Creates keynumber map with restricted range. Out of bounds arguments
   are transposed by octaves until they are in bounds.

   name - instrument name
   lower - MIDI key number
   upper - MIDI key number."
  `(let ((count (- ,upper ,lower)))
     (param ,name nil)
     (setf (symbol-function ',name)
	   #(lambda (kn)
	      (if (eq kn :?)
		  (prog
		   (format t "~A reduced-range keymap [~A ~A]~%"
			   ,name ,lower ,upper)
		   ,lower)
		(let ((k (keynumber kn)))
		  (while (< k ,lower)
		    (setf k (+ 12 k)))
		  (while (> k ,upper)
		    (setf k (- k 12)))
		  k))))))
