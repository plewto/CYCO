;;;; emu procussion heavy-handed
;;;;
;;;; Zone Stack
;;;;  1 019 Kicker        : 036 036                                                               
;;;;  2                   :                                                                    
;;;;  3 098 SmackSnare    : 038                                                               
;;;;  4 102 Dans Snare    : 040                                                                 
;;;;  5 167 BckwrdSnr2    : 037                                
;;;;  6                   :                                                                    
;;;;  7 270 Beasty Hat    : 042                                                             
;;;;  8 271 BeastyOpen    : 044                                                             
;;;;  9 236 HiHatAstmp    : 039                                                            
;;;; 10                   :                                                                      
;;;; 11 194 Power Toms    : 041                                                                
;;;; 12 194 Power Toms    : 043                                                                
;;;; 13 194 Power Toms    : 045                                                                 
;;;; 14 194 Power Toms    : 047                                   
;;;; 15 512 Bent China    : 046                                                             
;;;; 16 288 16"Crash 2    : 049                                      
;;;; 17 289 19" Pang      : 051                                                   
;;;; 18 289 19" Pang      : 054                                                   
;;;; 19                   :                                                      
;;;; 20 194 Power Toms    : 048                                          
;;;; 21 195 PowerTom 1    : 053                                          
;;;; 22 202 EleckTom      : 052                                          
;;;; 23 202 ElectTom      : 050                                          
;;;; 24 200 Epic Tom      : 055 096
;;;;

(in-package :cyco)
(param heavy-handed nil)
(let ((kick-map (keymap 'kick '((A 36))))
      (snare-map (keymap 'snare '((A 38 "Smack")
				  (B 40 "Dans")
				  (C 37 "Bakwrd"))))
      (hat-map (keymap 'hat '((A 42 "Beasty")
			      (B 44 "Beasty Open")
			      (C 39 "HiHat A stomp"))))
      (power-tom-map (keymap 'power-tom '((A 41)
					  (B 43)
					  (C 45)
					  (D 47)
					  (E 48))))
      (cymbal-map (keymap 'cymbal-map '((A 46 "Bent China")
					(B 49 "16 Crash")
					(C 51 "19 Pang")
					(D 54 "19 Pang"))))
      (electro-tom-map (keymap 'electro-tom '((A 53)
					      (B 53)
					      (C 50))))
      (epic-tom-map (circular-keymap 'epic-tom (range 55 96)))
      (program-number (car (cdr (assoc 'heavy-handed +PROCUSSION-PROGRAMS+)))) )
  (defun heavy-handed (&key (parent pro3))
    (setf heavy-handed (create-instrument 'heavy-handed
					  :parent parent
					  :transient t
					  :program-change-hook
					  (constant-program-hook
					   'heavy-handed program-number)))
    (param hhand-kick (create-instrument 'hhand-kick
					 :parent heavy-handed
					 :keynumber-map kick-map))
    (param hhand-snare (create-instrument 'hhand-snare
					  :parent heavy-handed
					  :keynumber-map snare-map))
    (param hhand-hat (create-instrument 'hhand-hat
					:parent heavy-handed
					:keynumber-map hat-map))
    (param hhand-power-tom (create-instrument 'hhand-power-tom
					      :parent heavy-handed
					      :keynumber-map power-tom-map))
    (param hhand-cymbal (create-instrument 'hhand-cymbal
					   :parent heavy-handed
					   :keynumber-map cymbal-map))
    (param hhand-electro-tom (create-instrument 'hhand-electro-tom
						:parent heavy-handed
						:keynumber-map electro-tom-map))
    (param hhand-epic-tom (create-instrument 'hhand-epic-tom
					     :parent heavy-handed
					     :keynumber-map epic-tom-map))
    heavy-handed))
