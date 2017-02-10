;;;; cyco/Local/Emu/Procussion/intervallix.lisp
;;;; 2013.04.22  
;;;; Instruments:
;;;;    intervallix
;;;;        intervallix-snap
;;;;        intervallix-tone
;;;;        intervallix-cow
;;;;
;;;; 01 503 Inner val      : 036 - 048 : tune +00 +00
;;;; 02 506 WholeTone2     : 048 - 060 : tune +00 +00
;;;; 03 504 Fibersnap      : 060 - 072 : tune +00 +00
;;;; 04 505 WholeTone1     : 072 - 084 : tune +00 +00 
;;;; 05 506 WholeTone2     : 084 - 096 : tune +00 +00
;;;; 06 502 Macho Cow      : 036 - 036 : tune -12 +00
;;;; 07 502 Macho Cow      : 096 - 096 : tune +00 +00
;
;       3         4         5         6         7         8         9         1 
;       01234567890123456789012345678901234567890123456789012345678901234567890123456789
; IV          *************                                                             
; WT2                     *************                                                
; FS                                  *************                                    
; WT1                                             *************
; MC          *                                                           *
;       01234567890123456789012345678901234567890123456789012345678901234567890123456789
;       3         4         5         6         7         8         9         1 


(defun --invalid-intvalx-keynum (iname kn)
  (let ((msg (format nil "Invalid intervallix-~A keynumber ~A" iname kn)))
    (cyco-warning msg)
    0))

(defun --intvalx-intvalx-map (k)
  (cond ((integerp k)
	 (while (< 36)(setf k (+ 12 k)))
	 (while (> 48)(setf k (- 12 k)))
	 k)
	((numberp k)
	 (--intvalx-intvalx-map (truncate k)))
	((eq k '?)(format t "036 - 048~%") 0)
	((and k (symbolp k))(--intvalx-intvalx-map (keynum k)))
	(t (--invalid-intvalx-keynum "" k))))

(defun --intvalx-snap-map (k)
  (cond ((integerp k)
	 (while (< 60)(setf k (+ 12 k)))
	 (while (> 72)(setf k (- 12 k)))
	 k)
	((numberp k)
	 (--intvalx-snap-map (truncate k)))
	((eq k '?)(format t "060 - 072~%") 0)
	((and k (symbolp k))(--intvalx-snap-map (keynum k)))
	(t (--invalid-intvalx-keynum "snap" k))))


(constant +intvalx-tone-keynumbers+
	  (append (range-between 48 61)
		  (range-between 72 85)))

(defun --intvalx-tone-map (k)
  (cond ((integerp k)
	 (cnth k +intvalx-tone-keynumbers+))
	((numberp k)(--intvalx-tone-map (truncate k)))
	((eq k '?)(format t "~A~%" +intvalx-tone-keynumbers+))
	(t (--invalid-intvalx-keynum "tone" k))))

(param intervallix nil)
(defun intervallix (&optional (parent pro4))
  (remove-children parent)
  (set-value parent :program (procussion-program-map 'intervallix))
  (let* ((in (instrument :intervallix
			 :parent parent
			 :keymap #'--intvalx-intvalx-map))
	 (sn (instrument :intervallix-snap
			 :parent in
			 :keymap #'--intvalx-snap-map))
	 (tn (instrument :intervallix-tone
			 :parent in
			 :keymap #'--intvalx-tone-map))
	 (cow (instrument :intervallix-cow
			  :parent in
			  :keymap #'(lambda (k)
				      (cond ((numberp k)
					     (if (oddp (truncate k)) 96 36))
					    ((eq k '?)(format t "0 1~%") 0)
					    ((null k) 36)
					    (t 96))))))
    (setf intervallix in)
    (param intervallix-snap sn)
    (param intervallix-tone tn)
    (param intervallix-cow cow)
    in))
	 
