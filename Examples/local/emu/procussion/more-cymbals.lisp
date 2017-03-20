;;;; emu procussion more-cymbals
;;;;
;;;; Creates main instrument and 3 sub instruments
;;;;
;;;;   more-cymbals
;;;;      mc-hats     - house hats
;;;;      mc-beasty   - hats with an edge
;;;;      mc-cymbals  - 2 crash and a pang ccymbals
;;;;
;;;;
;;;; Zone Stack
;;;;  1 267 HouseHat 5    : 036 041                                                               
;;;;  2 268 HouseHat 6    : 041 046                                                            
;;;;  3 244 ClsTipHatA    : 046 051                                                           
;;;;  4 245 OpenHiHatA    : 051 056                                                             
;;;;  5 277 Hatsoff       : 056 061                            
;;;;  6 270 Beasty Hat    : 061 066                                                            
;;;;  7 271 BeastyOpen    : 066 071                                                         
;;;;  8 317 StereoMalt    : 071 076                                                         
;;;;  9 297 Cymb.Decay    : 076 081                                                        
;;;; 10 299 PangCymbal    : 081 086                                                              
;;;; 11 296 Dubl Crash    : 086 096                                                            
;;;; 12 312 SFX 2         : 098 098                                                            
;;;; 13                   :                                                                     
;;;; 14                   :                                       
;;;; 15                   :                                                                 
;;;; 16                   :                                          
;;;; 17                   :                                                       
;;;; 18                   :                                                       
;;;; 19                   :                                                      
;;;; 20                   :                                              
;;;; 21                   :                                              
;;;; 22                   :                                              
;;;; 23                   :                                              
;;;; 24                   :         
;;;;

(in-package :cyco)
(param more-cymbals nil)
(let*((program-number (car (cdr (assoc 'more-cymbals +PROCUSSION-PROGRAMS+))))
      (hats-list '())
      (beasty-list '())
      (cymb-list '()))
  (dotimes (counter 5)
    (dolist (spec '((X 36 "HouseHat 5")
		    (TIP 46 "ClsTipHatA")
		    (OPN 41 "HouseHat 6")
		    (OPEN 51 "OpenHiHatA")))
      (let ((name (if (zerop counter)
		      (car spec)
		    (intern (str+ (car spec) counter))))
	    (value (+ counter (second spec))))
	(push (list name value (third spec)) hats-list)))
    (dolist (spec '((X       56 "Hatsoff")
		    (BEASTY 61 "Beasty Hat")
		    (Open 66 "Beasty Open")))
      (let ((name (if (zerop counter)
		      (car spec)
		    (intern (str+ (car spec) counter))))
	    (value (+ counter (second spec))))
	(push (list name value (third spec)) beasty-list)))
    (dolist (spec '((X 76 "Cymb.Decay")
		    (Pang 81 "PangCymbal")
		    (Crash 86 "Dubl Crash")))
      (let ((name (if (zerop counter)
		      (car spec)
		    (intern (str+ (car spec) counter))))
	    (value (+ counter (second spec))))
	(push (list name value (third spec)) cymb-list))))
  (push (list 'CRASH5 91 "Dubl Crash") cymb-list)
  (push (list 'SFX 98 "SFX 2") cymb-list)
  
  (defun more-cymbals (&key (parent pro2))
    (setf more-cymbals (create-instrument 'more-cymbals
					 :parent parent
					 :transient t
					 :program-change-hook
					 (constant-program-hook
					  'more-cymbals program-number)))
    (param mc-hats (create-instrument 'mc-hats
				      :parent more-cymbals
				      :keynumber-map
				      (keymap 'mc-hats (reverse hats-list))))
    (param mc-beasty (create-instrument 'mc-beasty
					:parent more-cymbals
					:keynumber-map
					(keymap 'mc-beasty (reverse beasty-list))))
    (param mc-cymbals (create-instrument 'mc-cymbals
					 :parent more-cymbals
					 :keynumber-map
					 (keymap 'mc-cymbals (reverse cymb-list))))
    more-cymbals))
  
