;;;; cyco.comp.keynum
;;;;
;;;; Keynumbers may be expressed as integer MIDI key numbers between 
;;;; 0 and 127 inclusive, or symbolically.
;;;; Symbolic keynumbers have the form: Q[M]O
;;;; Where Q is one of A B C D E F or G.
;;;;       M is optional accidental, either F (flat) or S (sharp)
;;;;       0 is integer octave number 0..10.
;;;; Enharmonic values BS, CF, ES and FF are defined.
;;;; Whenever conversion is made from an integer to a symbolic keynumber
;;;; the sharp version is used.   61 -> CS5 not DF5, though both CS5
;;;; and DF5 have the value 61.
;;;;
;;;; The special key number 'R indicates a rest and maps to -1.  All
;;;; negative values map to R.
;;;;


(in-package :cyco)

(constant +KEY-NUMBERS+
	  (flet ((make-sym (a b)
			   (intern (format nil "~A~A" a b))))
	    (let* ((symlist #((C BF)(CS DF)(D D)(DS EF)
			      (E FF)(F ES)(FS GF)(G G)
			      (GS AF)(A A)(AS BF)(B CF)))
		   (acc (make-hash-table :size 215)))
	      (dotimes (keynum 128)
		(let* ((pclass (rem keynum 12))
		       (octave (truncate (/ keynum 12)))
		       (syms (aref symlist pclass))
		       (primary (make-sym (first syms) octave))
		       (secondary (make-sym (second syms) octave)))
		  (setf (gethash primary acc) keynum)
		  (setf (gethash secondary acc) keynum)))
	      (setf (gethash 'R acc) -1)
	      acc)))

(constant +REVERSE-KEY-NUMBERS+
	  (let* ((symlist #(C CS D DS E F FS G FS A AS B))
		 (ary (make-array 128 :element-type 'symbol :initial-element nil)))
	    (dotimes (keynum 128)
	      (let* ((pclass (rem keynum 12))
		     (octave (truncate (/ keynum 12)))
		     (sym (intern (format nil "~A~A" (aref symlist pclass) octave))))
		(setf (aref ary keynum) sym)))
	    ary))

(defmethod keynumber-p ((n fixnum))
  (< n 128))

(defmethod keynumber-p ((s symbol))
  (gethash s +KEY-NUMBERS+))

(defmethod keynumber ((n number))
  (limit (truncate n) -1 127))

;; (defmethod keynumber ((s symbol))
;;   (or (gethash s +KEY-NUMBERS+)
;;       (error (format n "~A is not a keynumber" s))))

(defmethod keynumber ((s symbol))
  (or (gethash s +KEY-NUMBERS+) s))

(defmethod keynumber ((lst list))
  (let ((acc '()))
    (dolist (e lst)
      (push (keynumber e) acc))
    (reverse acc)))

(defmethod rest-p ((n t))
  (and (keynumber-p n)
       (minusp (keynumber n))))

(defmethod keyname ((n number))
  (cond ((minusp n) 'R)
	((< n 128) (aref +REVERSE-KEY-NUMBERS+ (truncate n)))
	(t nil)))

(defmethod keyname ((s symbol))
  (if (keynumber-p s) s nil))

(defmethod keyname ((lst list))
  (let ((acc '()))
    (dolist (e lst)
      (push (keyname e) acc))
    (reverse acc)))

(defmethod pitch-class ((n number))
  (cond ((minusp n) 'R)
	(t (rem (truncate n) 12))))

(defmethod pitch-class ((s symbol))
  (pitch-class (keynumber s)))

(defmethod pitch-class ((lst list))
  (let ((acc '()))
    (dolist (e lst)
      (push (pitch-class e) acc))
    (reverse acc)))

(defmethod octave ((n number))
  (cond ((minusp n) -1)
	(t (truncate (/ n 12)))))

(defmethod octave ((s symbol))
  (octave (keynumber s)))

(defmethod octave ((lst list))
  (let ((acc '()))
    (dolist (e lst)
      (push (octave e) acc))
    (reverse acc)))


(defmethod transposable-p ((a list)) a)

(defmethod transposable-p ((v vector)) v)

(defmethod transposable-p ((n fixnum))(keynumber-p n))

(defmethod transposable-p ((s symbol))(keynumber-p s))

(defmethod transpose ((a fixnum)(x fixnum) &key (range '(0 127)))
  (let ((c (+ a x))
	(low (car range))
	(high (second range)))
    (while (> c high)
      (setf c (- c 12)))
    (while (< c low)
      (setf c (+ c 12)))
    c))

(defmethod transpose ((a symbol)(x fixnum) &key (range '(0 127)))
  (transpose (keynumber a) x :range range))

(defmethod transpose ((obj list)(x fixnum) &key (range '(0 127)))
  (let ((acc '()))
    (dolist (e obj)
      (push (transpose e x :range range) acc))
    (reverse acc)))

(defmethod transpose ((ary array)(x fixnum) &key (range '(0 127)))
  (let* ((count (length ary))
	 (acc (make-array count)))
    (dotimes (i count)
      (setf (aref acc i)(transpose (aref ary i) x :range range)))
    acc))

(defmethod transpose! ((a t)(x fixnum) &key (range '(0 127)))
  (transpose a x :range range))

(defmethod transpose! ((a list)(x fixnum) &key (range '(0 127)))
  (dotimes (i (length a))
    (setf (nth i a)(transpose! (nth i a) x :range range)))
  a)

(defmethod transpose! ((a vector)(x fixnum) &key (range '(0 127)))
  (dotimes (i (length a))
    (setf (aref a i)(transpose! (aref a i) x :range range)))
  a)

(defmethod invert ((a t)(pivot t) &key (range '(0 127)))
  (let* ((p (keynumber pivot))
	 (q (keynumber a))
	 (delta (- p q))
	 (rs (+ p delta))
	 (low (car range))
	 (high (second range)))
    (while (< rs low)
      (setf rs (+ rs 12)))
    (while (> rs high)
      (setf rs (- rs 12)))
    rs))
	 
(defmethod invert ((a list)(pivot t) &key (range '(0 127)))
  (let ((acc '()))
    (dolist (e a)
      (push (invert e pivot :range range) acc))
    (reverse acc)))

(defmethod invert ((a vector)(pivot t) &key (range '(0 127)))
  (let* ((count (length a))
	 (acc (make-array count)))
    (dotimes (i count)
      (setf (aref acc i)(invert (aref a i) pivot :range range)))
    acc))

(defmethod invert! ((a t)(pivot t) &key (range '(0 127)))
  (invert a pivot :range range))

(defmethod invert! ((lst list)(pivot t) &key (range '(0 127)))
  (dotimes (i (length lst))
    (setf (nth i lst)(invert! (nth i lst) pivot :range range)))
  lst)

(defmethod invert! ((a vector)(pivot t) &key (range '(0 1277)))
  (dotimes (i (length a))
    (setf (aref a i)(invert! (aref a i) pivot :range range)))
  a)
