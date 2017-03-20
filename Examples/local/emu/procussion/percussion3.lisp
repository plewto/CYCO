;;;; emu procussion percussion3
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

(in-package :cyco)

(param percussion3 nil)
(let ((drum-map (keymap 'p3-drum '((rdrum    . 36)
				   (tdrum    . 38)
				   (hand     . 40)
				   (log      . 41)
				   (borine   . 47)
				   (cymdrum  . 48)
				   (stick    . 50)
				   (sdrum    . 52)
				   (stick2   . 53)
				   (spdrum   . 55))))
      (shaker-map (keymap 'p3-shaker '((tam    . 037)
				       (cast   . 044)
				       (guiro  . 045)
				       (baby   . 049)
				       (rasp   . 051)
				       (cool   . 054)
				       (cabasa . 098))))
      (bell-map (keymap 'p3-bell  '((tri     . 039)
				    (mbell   . 042)
				    (kalimba . 043)
				    (cow     . 046))))
      (shaker-drum-map (circular-keymap 'p3-shaker-drum (range 57 71)))
      (shaker-bell-map (circular-keymap 'p3-shakerbell (range 72 96)))
      (program-number (car (cdr (assoc 'percussion3 +PROCUSSION-PROGRAMS+)))) )
  (setf percussion3 nil)

  (defun percussion3 (&key (parent pro3))
    (setf percussion3 (create-instrument 'percussion3
					 :parent parent
					 :transient t
					 :program-change-hook (constant-program-hook
							       'percussion3 program-number)))
  (param p3-drum (create-instrument 'p3-drum
				    :parent percussion3
				    :keynumber-map drum-map))
  (param p3-shaker (create-instrument 'p3-shaker
				      :parent percussion3
				      :keynumber-map shaker-map))
  (param p3-shaker-drum (create-instrument 'p3-shaker-drum
					   :parent percussion3
					   :keynumber-map shaker-drum-map))
  (param p3-shaker-bell (create-instrument 'p3-shaker-bell
					   :parent percussion3
					   :keynumber-map shaker-bell-map))
  (param p3-bell (create-instrument 'p3-bell
				    :parent percussion3
				    :keynumber-map bell-map))
  percussion3))

	
  
