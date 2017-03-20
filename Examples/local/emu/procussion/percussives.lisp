;;;; emu procussion percussives
;;;;
;;;; Zone Stack
;;;;  1 488 BAM BAM       : 036 047                                                               
;;;;  2 274 NoiseHat A    : 037 053                                                            
;;;;  3 064 TikKick       : 048 059                                                           
;;;;  4 336 Syn Scrtch    : 054 065                                                             
;;;;  5 352 Analog Tick   : 060 071                            
;;;;  6 253 HouseHat 1    : 066 077                                                            
;;;;  7 367 Woodblock     : 072 083                                                         
;;;;  8 308 FingerCymb    : 078 096                                                         
;;;;  9 450 VoxFreak 2    : 084 096                                                        
;;;; 10 312 SFX 2         : 098                                                                  
;;;; 11                   :                                                                    
;;;; 12                   :                                                                    
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
(param percussives nil)
(let ((program-number (car (cdr (assoc 'percussives 
				       +PROCUSSION-PROGRAMS+)))))
  (param percussives nil)
  (defun percussives (&key (parent pro3))
    (setf percussives (create-instrument 
		       'percussives
		       :parent parent
		       :transient t
		       :program-change-hook
		       (constant-program-hook
			'percussives program-number)
		       :keynumber-map (circular-keymap 
				       'percussives
				       (cons (range 36 96) '(98)))))
    percussives))
