;;;; cyco.patterens.pattern
;;;;
;;;; 

(in-package :cyco)


;;; ---------------------------------------------------------------------- 
;;;			       PATTERN class
;;;
;;; Superclass for all patterns.
;;; Calling the next-1 or next methods on a PATTERN causes it to return
;;; the next value(s) in some prescribed way.   A PATTERN way contain other
;;; PATTERNS.

(defclass pattern nil
  ((elements
    :type list
    :accessor elements
    :initform '()
    :initarg :of)
   (current-value
    :type t
    :reader value
    :initform :not-a-datum)
   (pointer
    :type fixnum
    :accessor pointer
    :initform 0)))

(defmethod pattern-p ((obj pattern)) obj)

(defmethod clone ((p pattern) &key newname parent (hook #'identity))
  (dismiss newname parent)
  (funcall hook (make-instance (type-of p)
			       :of (clone (elements p)))))
    
(defmethod reset ((pat pattern))
  (setf (slot-value pat 'current-value) :not-a-datum)
  (setf (pointer pat) 0)
  (dolist (e (elements pat))(reset e))
  pat)

(defmethod cardinality ((pat pattern))
  (cardinality (elements pat)))

(defmethod remaining ((pat pattern))
  (let ((ptr (pointer pat))
	(mx (cardinality pat)))
    (- mx ptr)))

(defmethod next ((pat pattern) &optional (n 1))
  (cond 
   ((eq n :all)
    (elements pat))
   ((eq n :rest)
    (let ((n (remaining pat)))
      (next-n pat n)))
   ((eq n 1)
    (next-1 pat))
   ((integerp n)
    (next-n pat n))
   (t (next-1 pat))))

(defmethod ->string ((pat pattern))
  (format nil "~A :of ~A" (type-of pat)(elements pat)))

(defmethod filter! ((pat pattern) &key (test #'true))
  (setf (elements pat)(filter (elements pat) :test test))
  pat)

(defmethod filter ((pat pattern) &key (test #'true))
  (filter! (clone pat) :test test))

(defmethod transpose! ((p pattern)(x t) &key (range '(0 127)))
  (transpose! (elements p) x :range range)
  p)

(defmethod invert! ((p pattern)(pivot t) &key (range '(0 127)))
  (invert! (elements p) pivot :range range)
  p)

;;; ---------------------------------------------------------------------- 
;;;			    LINE pattern class
;;;
;;; A LINE is a PATTERN which returns it's values in sequence. Once the final
;;; value has been reached subsequent calls to next-1 continue to return the
;;; final value (at least until the line is reset).
;;;

(defclass line (pattern) nil)

(defun line (&key (of '()))
  (let ((pat (make-instance 'line :of of)))
    (reset pat)
    pat))

(defmethod next-1 ((pat line))
  (let ((p (min (1+ (pointer pat))
		(1- (cardinality pat)))))
    (let ((rs (next-1 (nth (pointer pat)(elements pat)))))
      (setf (pointer pat) p)
      (setf (slot-value pat 'current-value) rs)
      rs)))

;;; ---------------------------------------------------------------------- 
;;;                    cycle pattern class
;;;
;;; A CYCLE is a PATTERN which returns its values in a cyclical manner.
;;;

(defclass cycle (pattern) nil)

(defun cycle (&key (of '()))
  (reset (make-instance 'cycle :of of)))

(defmethod next-1 ((pat cycle))
  (let* ((p (rem (1+ (pointer pat))(cardinality pat)))
	 (rs (next-1 (nth (pointer pat)(elements pat)))))
    (setf (pointer pat) p)
    (setf (slot-value pat 'current-value) rs)
    rs))

;;; ---------------------------------------------------------------------- 
;;;			    HEAP pattern class
;;;
;;; A HEAP is a PATTERN which returns its elements in a random order without
;;; replacement.   Once all elements have been returned, the element order
;;; is shuffled.

(defclass heap (pattern) nil)

(defun heap (&key (of '()))
  (reset (make-instance 'heap :of of)))

(defmethod reset ((pat heap))
  (call-next-method)
  (setf (elements pat)
	(permutation (elements pat)))
  pat)

(defmethod next-1 ((pat heap))
  (let ((p (pointer pat)))
    (if (zerop p)
	(setf (elements pat)(permutation (elements pat))))
    (let ((rs (next-1 (nth p (elements pat)))))
      (setf (pointer pat)(rem (1+ p)(cardinality pat)))
      (setf (slot-value pat 'current-value) rs)
      rs)))

;;; ---------------------------------------------------------------------- 
;;;			     BAG pattern class
;;;
;;; A BAG is a PATTERN which returns it's elements at random with replacement.

(defclass bag (pattern) nil)

(defun bag (&key (of '()))
  (reset (make-instance 'bag :of of)))

(defmethod reset ((pat bag))
  (call-next-method)
  (setf (elements pat)(permutation (elements pat)))
  pat)

(defmethod next-1 ((pat bag))
  (next-1 (pick (elements pat))))



;;; Functions on Patterns
;;;

(defmethod ->pattern ((p pattern)) p)

(defmethod ->pattern ((lst list)) (cycle :of lst))

(defmethod ->pattern ((obj t)) (cycle :of (->list obj)))
