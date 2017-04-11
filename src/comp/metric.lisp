;;;; cyco.comp.metric
;;;;
;;;; Defines metric time values, mostly used for duration.
;;;; A metric value is a real non-negative number or one of the following
;;;; symbols.
;;;;
;;;;     W - whole
;;;;     H - half
;;;;     Q - quarter
;;;;     E - eight
;;;;     S - sixteenth
;;;;     T - thirty-second
;;;;     X - sixty-fourth
;;;;
;;;; These base values may be modified in one of several ways.
;;;;
;;;;     .   - dotted   Q, -> dotted quarter note.
;;;;     ..  - double dotted.
;;;;     ... - triple dotted
;;;;     T   - triplet QT -> quarter note triplet.
;;;;     5   - 5-tuplet Q5 -> 4/5 time of regular quarter note
;;;;     +   - add sixty-forth note   Q+   -> quarter note tied to sixty-fourth
;;;;     -   - subtract sixty-fourth note   Q-
;;;;

(in-package :cyco)

(constant +METRIC-UNITS+
	  (let ((ref '((W . 4)(H . 2)(Q . 1)(E . 1/2)
		       (S . 1/4)(T . 1/8)(X . 1/16)))
		(acc (make-hash-table :size 49)))
	    (flet ((extend (frmt scale shift)
			   (dolist (base ref)
			     (let* ((base-name (->string (car base)))
				    (base-value (cdr base))
				    (new-sym (intern (format nil frmt base-name)))
				    (new-value (+ (* base-value scale) shift)))
			       (setf (gethash new-sym acc) new-value)))))
	      (extend "~A" 1 0)
	      (extend "~A." 3/2 0)
	      (extend "~A.." 7/4 0)
	      (extend "~A..." 21/8 0)
	      (extend "~AT" 2/3 0)
	      (extend "~A5" 4/5 0)
	      (extend "~A+" 1 1/64)
	      (extend "~A-" 1 -1/64)
	      (setf (gethash 'R acc) -1)
	      acc)))

(defmethod metric-p ((s symbol))
  (gethash s +METRIC-UNITS+))

(defmethod metric-p ((n number))
  (>= n 0))

(defmethod metric ((n number))
  (float (max n 0)))

;; (defmethod metric ((s symbol))
;;   (or (gethash s +METRIC-UNITS+)
;;       (error (format nil "~A is not a valid metric value." s))))

(defmethod metric ((s symbol))
  (or (float (gethash s +METRIC-UNITS+)) s))

(defmethod metric ((s string))
  (metric (intern (string-upcase s) :cyco)))

(defmethod metric ((lst list))
  (let ((acc '()))
    (dolist (e lst)
      (push (metric e) acc))
    (reverse acc)))

(defmethod tie+ ((lst list))
    (apply #'+ (metric lst)))
	
(defmethod tie- ((lst list))
  (apply #'- (metric lst)))


(defgeneric parse-metric-symbol (s)
  (:documentation
   "Convert metric expression to time.
  Expression must have form:  [n*]a+b+c...
  Where n is a number
  and   a, b, c are valid metric values.

  There may be zero or one multiplicative term,
  and any number of additive terms.  The value n
  is multiplied by the sum of the additive terms:

  n*(a+b+c)  

  If n defaults to 1.

  Some examples:
  4 whole notes   4*w
  5 dotted qurter notes  5*q+e  or 5*q.
  A double dotted quarter note  q+e+s"))

(defgeneric parse-metric-expression (s))

(defmethod parse-metric-expression ((s string))
  (let* ((foo (split-string s #\*))
	 (mult 1)
	 (acc 0))
    (if (> (length foo) 1)
    	(progn
    	  (setf mult (read-from-string (car foo)))
    	  (setf foo (cdr foo))))
    (dolist (term1 foo)
      (dolist (term2 (split-string term1 #\+))
	(setf acc (+ acc (metric term2)))))
    (* mult acc)))
    
(defmethod parse-metric-expression ((s symbol))
  (parse-metric-expression (symbol-name s)))
      
