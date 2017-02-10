;;;; cyco/Local/Emu/Procussion/percussion3.lisp
;;;; 2013.04.22
;;;; Instruments
;;;;    percussion3    
;;;;        drum3
;;;;        shaker3
;;;;        cow3
;;;;        shaker-drum3
;;;;        shaker-bell3
;;;;
;;;; Zone   Stack            Key range
;;;; 01     :508 RitualDrum  :036 - xxx   : drum
;;;; 03     :423 Tone Drum   :038 - xxx   : drum
;;;; 04     :351 hand Drum   :040 - xxx   : drum
;;;; 06     :377 Log Drum    :041 - xxx   : drum
;;;; 13     :444 Drumdorine  :047 - xxx   : drum
;;;; 14     :509 CymbalDrum  :048 - xxx   : drum
;;;; 15     :539 The Stick   :050 - xxx   : drum
;;;; 17     :443 ShakerDrum  :052 - xxx   : drum
;;;; 19     :371 Ambi-Stick  :053 - xxx   : drum
;;;; 21     :534 SpaceBlock  :055 - xxx   : drum
;;;;
;;;; 02     :368 Tambourine  :037 - xxx   : shaker
;;;; 08     :366 Castenet    :044 - xxx   : shaker
;;;; 11     :392 Guiro Down  :045 - xxx   : shaker
;;;; 12     :511 BabyBreath  :049 - xxx   : shaker
;;;; 16     :554 Rasparity   :051 - xxx   : shaker
;;;; 18     :535 Cool FX     :054 - xxx   : shaker
;;;; 20     :538 The Shaker  :056 - xxx   : shaker
;;;; 24     :389 cabasaFrnt  :098 - 098   : shaker
;;;;
;;;; 05     :364 HiTriangle  :039 - xxx   : bell
;;;; 07     :464 MelodicBel  :042 - xxx   : bell
;;;; 10     :455 Kalimba     :043 - xxx   : bell
;;;; 09     :370 Cow Block   :046 - xxx   : bell
;;;;
;;;; 22     :457 ShakerDrm2  :057 - 071   : tune-drum
;;;; 23     :465 ShakerBell  :072 - 096   : tune-bell
;;;;

(defmap --percussion3-drum-map '((rdrum    . 36)
				 (tdrum    . 38)
				 (hand     . 40)
				 (log      . 41)
				 (borine   . 47)
				 (cymdrum  . 48)
				 (stick    . 50)
				 (sdrum    . 52)
				 (stick2   . 53)
				 (spdrum   . 55)))

(defmap --percussion3-shaker-map '((tam    . 037)
				   (cast   . 044)
				   (guiro  . 045)
				   (baby   . 049)
				   (rasp   . 051)
				   (cool   . 054)
				   (cabasa . 098)))

(defmap --percussion3-bell-map   '((tri     . 039)
				   (mbell   . 042)
				   (kalimba . 043)
				   (cow     . 046)))

(defun --percussion3-shaker-drum-map (k)
  (cond ((and (numberp k)(minusp k)) 0)
	((numberp k)
	 (+ 57 (rem (truncate k) 14)))
	((eq k '?)(format t "0 - 14~%") 0)
	((eq k 'r) 0)
	((and k (symbolp k))(keynum k))
	(t 0)))

(defun --percussion3-shaker-bell-map (k)
  (cond ((and (numberp k)(minusp k)) 0)
	((numberp k)
	 (+ 72 (rem (truncate k) 24)))
	((eq k '?)(format t "0 - 24~%") 0)
	((eq k 'r) 0)
	((and k (symbolp k))(keynum k))
	(t 0)))

(param percussion3 nil)

(defun percussion3 (&optional (parent pro3))
  (remove-children parent)
  (set-value parent :program (procussion-program-map 'percussion3))
  (let* ((p3 (instrument 'percussion3
			 :parent parent)))
    (setf percussion3 p3)
    (param drum3 (instrument :drum3
			     :parent p3
			     :keymap #'--percussion3-drum-map))
    (param shaker3 (instrument :shaker3
			       :parent p3
			       :keymap #'--percussion3-shaker-map))
    (param cow3 (instrument :cow3
			    :parent p3
			    :keymap #'--percussion3-bell-map))
    (param shaker-drum3 (instrument :shaker-drum3
				    :parent p3
				    :keymap #'--percussion3-shaker-drum-map))
    (param shaker-bell3 (instrument :shaker-bell3
				    :parent p3
				    :keymap #'--percussion3-shaker-bell-map))
    p3))

	
  
