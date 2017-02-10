;;;; cyco/Local/Emu/Procussion/latin-layers
;;;; 2013.04.22
;;;; Instruments
;;;;    latin-layers
;;;;        ll-timbale
;;;;        ll-hand
;;;;        ll-clave
;;;;        ll-tumba
;;;;        ll-quinto
;;;;        ll-macho
;;;;        ll-hat
;;;;
;;;; ZN : STACK             : KEY       : TUNE :
;;;; 01 : 399 Timbale 1b    : 036 - 047 :      : timbale
;;;; 02 : 368 Tambourine    : 036 - 053 :      : 
;;;; 03 : 351 hand Drum     : 048 - 059 :      : hand
;;;; 04 : 396 ShakerBack    : 054 - 065 :      : 
;;;; 05 : 365 Clave         : 060 - 071 :      : clave 60 - 71
;;;; 06 : 392 Guiro Down    : 066 - 077 :      :
;;;; 07 : 404 Tumba Tone    : 072 - 074 :      : tumba 72 - 77
;;;; 08 : 394 ShakerNet     : 078 - 096 :      :
;;;; 09 : 405 Tumba Rim     : 075 - 077 :      : tumba 75 - 77
;;;; 10 : 406 Quinto Tone   : 078 - 080 :      : quinto
;;;; 11 : 407 Quinto SlapO  : 081 - 083 :      : quinto
;;;; 12 : 408 Quinto SlapC  : 084 - 086 :      : quinto
;;;; 13 : 413 MachoTip L    : 087 - 089 :      : macho
;;;; 14 : 414 MachoTip R    : 090 - 092 :      : macho
;;;; 15 : 233 HihatA 1/3    : 098       :      : hat
;;;; 16 : 233 HiHatA 1/3    : 062       :      :
;;;; 17 : 414 MachTip R     : 100       :      : macho
;;;; 18 : 416 Macho Tone    : 101       :      : macho
;;;; 19 : 341 RevSnapper    : 103       :      : hat
;;;; 20 : xxx               : xxx - xxx :      :
;;;; 21 : xxx               : xxx - xxx :      :
;;;; 22 : xxx               : xxx - xxx :      :
;;;; 23 : xxx               : xxx - xxx :      :
;;;; 24 : xxx               : xxx - xxx :      :
;
;        3   4         5         6         7         8         9         0         
;        678901234567890123456789012345678901234567890123456789012345678901
; 1 tim  678901234567
; 2 tam  678901234567890123
; 3 hand             890123456789
; 4 shak                   456789012345
; 5 clav                         012345678901
; 6 guir                               678901234567
; 7 tum                                      2345
; 8 net                                            8901234567890123456
; 9 tum                                         567
; 0 qun                                            890
; 1 qun                                               123
; 2 qun                                                  456
; 3 mac                                                     789
; 4 mac                                                        012
; 5 hat                                                                8
; 6 hat                            2
; 7 mac                                                                  0
; 8 mac                                                                   1
; 9 snap                                                                    3
;        678901234567890123456789012345678901234567890123456789012345678901
;        3   4         5         6         7         8         9         0         


(defun --ll-timbale-map (k)
  (cond ((eq k 'r) 0)
	((eq k '?)(format t "000 - 011~%") 0)
	((numberp k)
	 (+ 36 (rem (truncate k) 11)))
	((and k (symbolp k))(--ll-timbale-map (keynum k)))
	(t 0)))

(defun --ll-hand-map (k)
  (cond ((eq k 'r) 0)
	((eq k '?)(format t "000 - 011~%") 0)
	((numberp k)
	 (+ 48 (rem (truncate k) 11)))
	((and k (symbolp k))(--ll-hand-map (keynum k)))
	(t 0)))

(defun --ll-clave-map (k)
  (cond ((eq k 'r) 0)
	((eq k '?)(format t "000 - 011~%") 0)
	((numberp k)
	 (+ 60 (rem (truncate k) 11)))
	((and k (symbolp k))(--ll-clave-map (keynum k)))
	(t 0)))

(defun --ll-tumba-map (k)
  (cond ((eq k 'r) 0)
	((eq k '?)(format t "000 - 006~%") 0)
	((numberp k)
	 (+ 72 (rem (truncate k) 6)))
	((and k (symbolp k))(--ll-tumba-map (keynum k)))
	(t 0)))

(defun --ll-quinto-map (k)
  (cond ((eq k 'r) 0)
	((eq k '?)(format t "000 - 008~%") 0)
	((numberp k)
	 (+ 78 (rem (truncate k) 8)))
	((and k (symbolp k))
	 (--ll-quinto-map (keynum k)))
	(t 0)))

(defun --ll-macho-map (k)
  (cond ((eq k 'r) 0)
	((eq k '?)(format t "000 - 008~%") 0)
	((numberp k)
	 (cnth (truncate k) '(100 101 87 90 88 91 89 92)))
	((and k (symbolp k))
	 (--ll-macho-map (keynum k)))
	(t 0)))

(defun --ll-hat-map (k)
  (cond ((eq k 'r) 0)
	((eq k '?)(format t "000 - 001~%") 0)
	((numberp k)
	 (if (oddp (truncate k)) 98 103))
	(t (if k 98 103))))


(param latin-layers nil)

(defun latin-layers (&optional (parent pro3))
  (remove-children parent)
  (set-value parent :program (procussion-program-map 'latin-layers))
  (let ((ll (instrument :latin-layers
			:parent parent)))
    (param ll-timbale (instrument :ll-timbale 
				  :parent ll
				  :keymap #'--ll-timbale-map))
    (param ll-hand (instrument :ll-hand
			       :parent ll
			       :keymap #'--ll-hand-map))
    (param ll-clave (instrument :ll-clave
				:parent ll
				:keymap #'--ll-clave-map))
    (param ll-tumba (instrument :ll-tumba
				:parent ll
				:keymap #'--ll-tumba-map))
    (param ll-quinto (instrument :ll-quinto
				 :parent ll
				 :keymap #'--ll-quinto-map))
    (param ll-macho (instrument :ll-macho
				:parent ll
				:keymap #'--ll-macho-map))
    (param ll-hat (instrument :ll-hat
			      :parent ll
			      :keymap #'--ll-hat-map))
    (setf latin-layers ll)
    latin-layers))
