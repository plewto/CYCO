;;;; emu procussion lotsa-kicks
;;;;
;;;; Zone Stack
;;;;  1 068 Earth Kick    : 036 039      36 37 38                                                 
;;;;  2 023 Bonzo Kick    : 039 042                                                            
;;;;  3 048 ModTonerap    : 042 045                                                           
;;;;  4 040 Kooky Kick    : 045 048                                                             
;;;;  5 036 Havok Kick    : 048 051                            
;;;;  6 028 Wet Kick 4    : 051 054                                                            
;;;;  7 012 Dry Kick 4    : 054 057                                                         
;;;;  8 037 Cave Kick     : 057 060                                                         
;;;;  9 069 BigBassDrum   : 060 063                                                        
;;;; 10 013 Dry Kick 5    : 063 066                                                              
;;;; 11 033 ModSnapKik    : 066 069                                                            
;;;; 12 051 Janz Kick     : 069 072                                                            
;;;; 13 054 Kick In       : 072 075                                                             
;;;; 14 035 Metal Kick    : 075 078                               
;;;; 15 020 Fat Kick      : 078 081                                                         
;;;; 16 014 Dry Kick 6    : 081 084                                  
;;;; 17 052 Mambo Kick    : 084 087                                               
;;;; 18 018 Round Kick    : 087 090                                               
;;;; 19 017 Gated Kick    : 090 093                                              
;;;; 20 053 RapperKick    : 093 096                                      
;;;; 21                   :                                              
;;;; 22                   :                                              
;;;; 23                   :                                              
;;;; 24                   :         
;;;;

(in-package :cyco)
(param lotsa-kicks nil)
(let*((program-number (car (cdr (assoc 'lotsa-kicks +PROCUSSION-PROGRAMS+))))
      (acc '()))
  (dotimes (counter 3)
    (dolist (spec '((A 036 "Earth Kick")
		    (B 039 "Bonzo Kick")
		    (C 042 "ModTonerap")
		    (D 045 "Kooky Kick")
		    (E 048 "Havok Kick")
		    (F 051 "Wet Kick 4")
		    (G 054 "Dry Kick 4")
		    (H 057 "Cave Kick")
		    (I 060 "BigBassDrum")
		    (J 063 "Dry Kick 5")
		    (K 066 "ModSnapKik")
		    (L 069 "Janz Kick")
		    (M 072 "Kick In")
		    (N 075 "Metal Kick")
		    (O 078 "Fat Kick")
		    (P 081 "Dry Kick 6")
		    (Q 084 "Mambo Kick")
		    (R 087 "Round Kick")
		    (S 090 "Gated Kick")
		    (T 093 "RapperKick")))
      (let ((name (if (zerop counter)
		      (car spec)
		    (intern (str+ (car spec) counter))))
	    (value (+ counter (second spec))))
	(push (list name value (third spec)) acc))))
  (defun lotsa-kicks (&key (parent pro3))
    (setf lotsa-kicks (create-instrument 'lotsa-kicks
					 :parent parent
					 :transient t
					 :program-change-hook
					 (constant-program-hook
					  'lotsa-kicks program-number)
					 :keynumber-map
					 (keymap
					  'lotsa-kicks (reverse acc))))
    lotsa-kicks))
  
