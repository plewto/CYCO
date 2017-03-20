;;;; emu procussion multi-fx
;;;;
;;;; Zone Stack
;;;;  1   554 Rasparity   : 036 040                                                               
;;;;  2   536 Qool Klang  : 041 041                                                            
;;;;  3   308 FingerCym   : 042 042                                                           
;;;;  4   440 Hallowell   : 043 043                                                             
;;;;  5   441 Star Tree   : 044 044                                                       
;;;;  6   447 WarbleWave  : 045 045                                                            
;;;;  7   461 Ganga Log   : 046 046                                                         
;;;;  8   442 Thundadome  : 047 052                                                         
;;;;  9   525 Noise FX    : 053 055                                                        
;;;; 10   474 TinkleTine  : 065 071                                                              
;;;; 11   448 TempleBell  : 056 061                                                            
;;;; 12   554 Rasparity   : 062 064                                                            
;;;; 13   442 Thundadome  : 072 083                                                             
;;;; 14   475 Bellhause   : 084 096                               
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

(in-package :cyco)

(param multi-fx nil)
(let (
      ;; (rasparity-map (circular-keymap 'rasparity (zip (range 36 40)(range 62 64))))
      ;; (efx-map (keymap 'efx '((QOOL-KLANG 41)
      ;; 			      (FINGER-SYM 42)
      ;; 			      (HALLOWELL 43)
      ;; 			      (STARTREE 44)
      ;; 			      (WARBLE 45)
      ;; 			      (GANGA 46))))
      (thundadome-map (circular-keymap 'thundadome (zip (range 47 52)(range 72 83))))
      (noise-map (circular-keymap 'noise (range 53 55)))
      (tine-map (circular-keymap 'tinkeltine (range 65 71)))
      (bell-map (circular-keymap 'bell (range 56 61)))
      (bellhause-map (reduced-keymap 'bellhause 84 96))
      (program-number (car (cdr (assoc 'multi-fx +PROCUSSION-PROGRAMS+)))) )
  (defun multi-fx (&key (parent pro3))
    (setf multi-fx (create-instrument 'multi-fx
				      :parent parent
				      :transient t
				      :program-change-hook
				      (constant-program-hook
				       'multi-fx program-number)))
    (param mfx-thundadome (create-instrument
			   'mfx-thundadome
			   :parent multi-fx
			   :keynumber-map thundadome-map))
    (param mfx-noise (create-instrument
		      'mfx-noise
		      :parent multi-fx
		      :keynumber-map noise-map))
    (param mfx-tine (create-instrument
		     'mfx-tine
		     :parent multi-fx
		     :keynumber-map tine-map))
    (param mfx-bell (create-instrument
		     'mfx-bell
		     :parent multi-fx
		     :keynumber-map bell-map))
    (param mfx-bellhause (create-instrument
			  'mfx-bellhause
			  :parent multi-fx
			  :keynumber-map bellhause-map))
    multi-fx))
