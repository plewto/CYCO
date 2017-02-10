;;;; cyco/Local/Emu/Procussion/hip-house.lisp
;;;; 2013.04.22
;;;; Instruiments:
;;;;    hip-house
;;;;        hh-kick
;;;;        hh-snare
;;;;        hh-hat
;;;;        hh-cow
;;;;        hh-noise
;;;;        hh-bongo1
;;;;        hh-bongo2
;;;;        hh-sawbas
;;;;
;;;; ZN : STACK             : KEY       : TUNE
;;;; 01 : 014 Dry Kick 6    : 036       :        : *kick 
;;;; 02 : 116 BakwardSnr1   : 047       :        : *revsnr              
;;;; 03 : 140 DanceSnare    : 038       :        : *snr                 
;;;; 04 : 151 TamboSnare    : 037       :        : *snr                 
;;;; 05 : 099 ModernSnare   : 040       :        : *snr
;;;; 06 : 167 BacwrdSnr2    : 048       :        : *revsnr              
;;;; 07 : 282 ServoHat1     : 042       :        : *hat                 
;;;; 08 : 283 ServoHat2     : 044       :        : *hat                 
;;;; 09 : 284 ServoHat3     : 046       :        : *hat ped                
;;;; 10 : 167 BckwrdSnr2    : 050       :        : *revsnr              
;;;; 11 : 373 Cowtombell    : 041       : -08    : *cow                    
;;;; 12 : 373 Cowtombell    : 043       :        : *cow                 
;;;; 13 : 373 Cowtombell    : 045       : +08    : *cow                    
;;;; 14 : 432 ModSawTone    : 072 - 096 :        : bass                
;;;; 15 : 453 Red Sky       : 049       :        : *noise
;;;; 16 : 454 JungleNite    : 051       :        : *noise
;;;; 17 : 169 BckwrdSnar4   : 052       :        : *revsnr              
;;;; 18 : 272 Horny hat     : 066 - 071 :        : *hat
;;;; 19 : 346 Wack Claps    : 039       :        : *snr              
;;;; 20 : 170 RevSnare 1    : 053       :        : *snr                 
;;;; 21 : 160 RimTinTin     : 054 - 056 :        : *snr
;;;; 22 : 422 BongoBoy      : 097 - 127 :        : *bongo1               
;;;; 23 : 337 ErsatScrch    : 057 - 059 : -12    : *noise
;;;; 24 : 422 BongoBoy      : 060 - 065 :        : *bongo2              
;;;;
;;;;     3         4         5         6         7         8         9         0         
;;;;     01234567890123456789012345678901234567890123456789012345678901234567890
;;;; 14                                            *************************
;;;; 18                                      ******
;;;; 21                          ***
;;;; 22                                                                     **** -->
;;;; 23                            ***
;;;; 24                                ******
;;;;     01234567890123456789012345678901234567890123456789012345678901234567890
;;;;     3         4         5         6         7         8         9         0         

(defun --hh-kick-map (k)
  (cond ((eq k 'r) 0)
	((eq k '?) (format t "36~%") 0)
	(t 36)))

(defmap --hh-snare-map '((dance . 038)
			 (tambo . 037)
			 (mod   . 040)
			 (clap  . 039)
			 (rim1  . 053)
			 (rim2  . 054)
			 (rim3  . 055)
			 (rim4  . 056)
			 (bkwr1 . 047)
			 (bkwr2 . 048)
			 (bkwr3 . 050)
			 (bkwr4 . 052)))

(defmap --hh-hat-map '((x     . 042)
		       (op    . 044)
		       (ped   . 046)
		       (horn1 .  066)
		       (horn2 .  067)
		       (horn3 .  068)
		       (horn4 .  069)
		       (horn5 .  070)
		       (horn6 .  071)))

(defmap --hh-cow-map '((A     .  041)
		       (B     .  043)
		       (C     .  045)))

(defmap --hh-noise-map '((red      . 049)
			 (night    . 051)
			 (scratch1 . 057)
			 (scratch2 . 058)
			 (scrstch3 . 059)))

(defun --hh-bongo1-map (k)
  (cond ((eq k 'r) 0)
	((eq k '?)(format t "0 - 5~%") 0)
	((numberp k)
	 (+ 60 (rem (truncate k) 5)))
	((and k (symbolp k))(--hh-bongo1-map (keynum k)))
	(t 0)))

(defun --hh-bongo2-map (k)
  (cond ((eq k 'r) 0)
	((eq k '?)(format t "0 - 30~%") 0)
	((numberp k)
	 (+ 97 (rem (truncate k) 30)))
	((and k (symbolp k))(--hh-bongo2-map (keynum k)))
	(t 0)))

(defun --hh-sawbass-map (k)
  (cond ((eq k 'r) 0)
	((eq k '?) (format t "036 - 060~%") 0)
	((and (numberp k)(>= k 36)(< k 61)) (truncate k))
	((and k (symbolp k))(--hh-sawbass-map (keynum k)))
	(t 0)))

(param hip-house nil)

(defun hip-house (&optional (parent pro4))
  (remove-children pro4)
  (set-value parent :program (procussion-program-map 'hip-house))
  (let* ((hh (instrument :hip-house
			 :parent parent)))
    (setf hip-house hh)
    (param hh-kick (instrument :hh-kick
			       :parent hh
			       :keymap #'--hh-kick-map))
    (param hh-snare (instrument :hh-snare
				:parent hh
				:keymap #'--hh-snare-map))
    (param hh-hat (instrument :hh-hat
			      :parent hh
			      :keymap #'--hh-hat-map))
    (param hh-cow (instrument :hh-cow
			      :parent hh
			      :keymap #'--hh-cow-map))
    (param hh-noise (instrument :hh-noise
				:parent hh
				:keymap #'--hh-noise-map))
    (param hh-bongo1 (instrument :hh-bongo1
				 :parent hh
				 :keymap #'--hh-bongo1-map))
    (param hh-bongo2 (instrument :hh-bongo2
				 :parent hh
				 :keymap #'--hh-bongo2-map))
    (param hh-sawbass (instrument :hh-sawbass
				  :parent hh
				  :keymap #'--hh-sawbass-map))
    hh))
