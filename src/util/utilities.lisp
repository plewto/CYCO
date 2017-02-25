;;;; cyco.util.utilities
;;;;

(in-package :cyco)

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
       ,@body))

(defmacro until (test &rest body)
  `(do ()
       (,test)
       ,@body))

(defun bool (obj)
  "(bool obj)
   Return canonical Boolean value nil or t."
  (if obj t nil))

(defun true (&rest ignore)
  "(true)  Ignore all arguments and return t"
  (dismiss ignore)
  t)

(defun false (&rest ignore)
  "(false)  Ignore all arguments and return nil"
  (dismiss ignore)
  nil)

(defun limit (n mn mx)
  "(limit n mn mx)
   Restrict value of n such that mn <= n <= mx"
  (max (min n mx) mn))

(defun xdump (ary offset &key (end nil)(pad "    "))
  "Generate hex-dump of array
   offset - location in array to begin dump.
   end - ending location, defaults to end of array"
  (let ((line-width 8)
	(terminal (or end (length ary)))
	(ptr offset))
    (while (< ptr terminal)
      (format t "~A[~4X] " pad ptr)
      (dotimes (i line-width)
	(let ((index (+ ptr i)))
	  (if (< index terminal)
	      (format t "~2X " (aref ary index)))))
      (format t "~%")
      (setf ptr (+ ptr line-width)))
    (format t "~%")))
  
(defun make-keyword (name)
  "Create keyword with given name."
  (values (intern (string-upcase (->string name)) "KEYWORD")))

(defmacro defclone (name obj &key (rename t) parent (hook #'identity))
  "Create clone of object and bind to symbol name.
   name    - Symbol
   obj     - Object 
   :rename - Bool, if true rename the cloned object to name.
             obj must have a 'name' slot. Default t
   :parent - If object node like, set parent of clone to parent.
             Otherwise use parent of source object.
   :hook   - Function called on clone  (lambda (x)) -> x'
             Default #'identity"
  `(let ((other (clone ,obj
		       :parent ,parent
		       :hook ,hook)))
     (if ,rename (setf (slot-value ,obj 'name) ',name))
     (param ,name other)
     other))

(defun rnd ()
  "Returns random float between 0.0 and 1.0"
  (random 1.0))

(defun coin (&optional (p 0.5)(heads t)(tails nil))
  "Returns heads with probability p, tails otherwise.
   If heads or tails are functions, the function is called when selected."
  (let ((rs (if (< (rnd) p) heads tails)))
    (if (functionp rs)
	(funcall rs)
      rs)))

(defun rnd-sign (&optional (p 0.5)(n 1.0))
  "Returns +n with probability 0.5, -n otherwise."
  (* n (coin p 1 -1)))

(defun approx (n &key (scale 0.1)(min -1e6)(max 1e6))
  "Returns a value approximately the same as n.
   scale - maximum ratio of result relative to n.
   min   - Lower limit of result
   max   - Upper limit of result."
  (let* ((v (rnd-sign 0.5 (random (* n scale)))))
    (limit (+ n v) min max)))
