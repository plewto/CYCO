;;;; cyco/Local/Emu/Procussion/multi-fx.lisp
;;;; 2013.04.22
;;;; Instruments
;;;;    multi-fx
;;;;        mfx-rasp
;;;;        mfx-noise
;;;;        mfx-tine
;;;;        mfx-thundadome
;;;;        mfx-bell
;;;;
;;;; ZN : STACK             : KEY       : TUNE
;;;; 01 : 554 Rasparity     : 036 - 040 :         : rasp1
;;;; 02 : 536 Qool KlanQ    : 041       :         : main
;;;; 03 : 308 FingerCymb    : 042       :         : main
;;;; 04 : 440 Hallowell     : 043       :         : main
;;;; 05 : 441 Star Tree     : 044       :         : main
;;;; 06 : 447 WarbleWavee   : 045       :         : main
;;;; 07 : 461 Ganga Log     : 046       :         : main
;;;; 08 : 442 Thundadome    : 047 - 052 :         : cow
;;;; 09 : 525 Noise FX      : 053 - 055 :         : main
;;;; 10 : 474 TinklTine     : 065 - 071 :         : tine
;;;; 11 : 448 TempleBell    : 056 - 061 :         : tine
;;;; 12 : 554 Rasparity     : 062 - 064 :         : main
;;;; 13 : 442 Thundadome    : 072 - 083 :         : thun
;;;; 14 : 475 Bellhause     : 084 - 096 :         : bell
;;;; 15 : xxx               : xxx       :         :
;;;; 16 : xxx               : xxx       :         :
;;;; 17 : xxx               : xxx       :         :
;;;; 18 : xxx               : xxx       :         :
;;;; 19 : xxx               : xxx       :         :
;;;; 20 : xxx               : xxx       :         :
;;;; 21 : xxx               : xxx       :         :
;;;; 22 : xxx               : xxx       :         :
;;;; 23 : xxx               : xxx       :         :
;;;; 24 : SFX 1             : 098       :         : main

(defun --mfx-rasp-map (k)
  (cond ((eq k 'r) 0)
	((eq k '?)(format t "0 4~%") 0)
	((numberp k)
	 (+ 36 (rem (truncate k) 5)))
	((and k (symbolp k))
	 (--mfx-rasp-map (keynum k)))
	(t 0)))

(defmap --mfx-noise-map '((rasp     . 62)
			  (rasp2    . 63)
			  (rasp3    . 64)
			  (klanq    . 41)
			  (finger   . 42)
			  (hallow   . 43)
			  (star     . 44)
			  (warble   . 45)
			  (log      . 46)
			  (cow      . 47)
			  (cow2     . 48)
			  (cow3     . 49)
			  (noise    . 53)
			  (noise2   . 54)
			  (noise3   . 55)
			  (sfx      . 98)))

(defmap --mfx-tine-map '((tine    . 65)
			 (tine2   . 66)
			 (tine3   . 67)
			 (tine4   . 68)
			 (tine5   . 69)
			 (tine6   . 70)
			 (tine7   . 71)
			 (temple  . 56)
			 (temple1 . 57)
			 (temple2 . 58)
			 (temple3 . 59)
			 (temple4 . 60)
			 (temple5 . 61)))

(defun --mfx-thundadome-map (k)
  (cond ((eq k 'r) 0)
	((eq k '?)(format t "0 - 11~%"))
	((numberp k)
	 (+ 72 (rem (truncate k) 12)))
	((and k (symbolp k))
	 (--mfx-thundadome-map (keynum k)))
	(t 0)))

(defun --mfx-bell-map (k)
  (cond ((eq k 'r) 0)
	((eq k '?)(format t "0 - 12~%"))
	((numberp k)
	 (+ 84 (rem (truncate k) 13)))
	((and k (symbolp k))
	 (--mfx-bell-map (keynum k)))
	(t 0)))

(param multi-fx nil)

(defun multi-fx (&optional (parent pro4))
  (remove-children pro4)
  (set-value parent :program (procussion-program-map 'multi-fx))
  (let ((mfx (instrument :multi-fx
			:parent parent)))
    (setf multi-fx mfx)
    (param mfx-rasp (instrument :mfx-rasp
				:parent mfx
				:keymap #'--mfx-rasp-map))
    (param mfx-noise (instrument :mfx-noise 
				:parent mfx
				:keymap #'--mfx-noise-map))
    (param mfx-tine (instrument :mfx-tine
			       :parent mfx
			       :keymap #'--mfx-tine-map))
    (param mfx-thundadome (instrument :mfx-thundadome
				     :parent mfx
				     :keymap #'--mfx-thundadome-map))
    (param mfx-bell (instrument :mfx-bell
			       :parent mfx
			       :keymap #'--mfx-bell-map))
    mfx))
