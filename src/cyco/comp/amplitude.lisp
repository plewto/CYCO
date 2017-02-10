;;;; cyco.comp.amplitude
;;;;
;;;; Defines functions on amplitude.  Amplitude is defined symbolically
;;;; or as a value between 0 and 1 inclusive.
;;;;
;;;; Symbolic amplitudes use the familiar music notation of
;;;; pppp ppp pp p mp mf f ff fff and ffff.  A finer gradation may be
;;;; defined by appending the above symbols with - or +.
;;;;   ... pp- < pp < pp+ < p- < p < p+ ...
;;;;

(in-package :cyco)

(flet ((create-amp-table ()
			 (let ((acc (make-hash-table :size 31)))
			   (setf (gethash nil acc) 0)
			   (setf (gethash 'R acc) 0/127)
			   (setf (gethash 'pppp- acc) 2/127)
			   (setf (gethash 'pppp acc) 6/127)
			   (setf (gethash 'pppp+ acc) 10/127)
			   (setf (gethash 'ppp- acc) 14/127)
			   (setf (gethash 'ppp acc) 19/127)
			   (setf (gethash 'ppp+ acc) 23/127)
			   (setf (gethash 'pp- acc) 27/127)
			   (setf (gethash 'pp acc) 32/127)
			   (setf (gethash 'pp+ acc) 36/127)
			   (setf (gethash 'p- acc) 40/127)
			   (setf (gethash 'p acc) 45/127)
			   (setf (gethash 'p+ acc) 49/127)
			   (setf (gethash 'mp- acc) 53/127)
			   (setf (gethash 'mp acc) 58/127)
			   (setf (gethash 'mp+ acc) 62/127)
			   (setf (gethash 'mf- acc) 66/127)
			   (setf (gethash 'mf acc) 71/127)
			   (setf (gethash 'mf+ acc) 75/127)
			   (setf (gethash 'f- acc) 79/127)
			   (setf (gethash 'f acc) 84/127)
			   (setf (gethash 'f+ acc) 88/127)
			   (setf (gethash 'ff- acc) 92/127)
			   (setf (gethash 'ff acc) 97/127)
			   (setf (gethash 'ff+ acc) 101/127)
			   (setf (gethash 'fff- acc) 106/127)
			   (setf (gethash 'fff acc) 110/127)
			   (setf (gethash 'fff+ acc) 114/127)
			   (setf (gethash 'ffff- acc) 118/127)
			   (setf (gethash 'ffff acc) 123/127)
			   (setf (gethash 'ffff+ acc) 127/127)
			   acc)))
  (constant +AMP-TABLE+ (create-amp-table)))

(defmethod amplitude-p ((sym symbol))
  "Predicate, test if object is a valid amplitude."
  (gethash sym +AMP-TABLE+))

(defmethod amplitude-p ((n number)) n)

(defun --prep-amp (n range)
  (let ((scale 1000.0))
    (limit (/ (round (* n scale)) scale)
	   (car range)
	   (cdr range))))

;; (defmethod amplitude ((sym symbol) &key (range '(0.0 . 1.0)))
;;   (let ((a (gethash sym +AMP-TABLE+)))
;;     (--prep-amp (or a (error (format nil "Invalid amplitude: ~A" sym)))
;; 		range)))

(defmethod amplitude ((sym symbol) &key (range '(0.0 . 1.0)))
  (let ((a (gethash sym +AMP-TABLE+)))
    (or (and a (--prep-amp a range)) sym)))

(defmethod amplitude ((n number) &key (range '(0.0 . 1.0)))
    (--prep-amp n range))

(defmethod amplitude ((lst list) &key (range '(0.0 . 1.0)))
  (let ((acc '()))
    (dolist (e lst)
      (push (amplitude e :range range) acc))
    (reverse acc)))
	    
(defmethod amplitude->velocity ((amp t) &key (range '(0.0 . 1.0)))
  (truncate (* 127 (amplitude amp :range range))))

(defmethod amplitude->velocity ((lst list) &key (range '(0.0 . 1.0)))
  (let ((acc '()))
    (dolist (e lst)
      (push (amplitude->velocity e :range range) acc))
    (reverse acc)))

(defmethod amplitude-symbol ((amp number))
  (let* ((symbols #(R pppp- pppp pppp+ ppp- ppp ppp+ pp- pp pp+ p- p p+
		      mp- mp mp+ mf- mf mf+
		      f- f f+ ff- ff ff+ fff- fff fff+ ffff- ffff ffff+))
	 (scount (length symbols))
	 (index (limit (truncate (* amp (1- scount))) 0 (1- scount))))
    (values (aref symbols index) index)))

(defmethod amplitude-symbol ((lst list))
  (let ((acc '()))
    (dolist (a lst)
      (push (amplitude-symbol a) acc))
    (reverse acc)))
