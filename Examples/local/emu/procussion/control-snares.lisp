;;;; emu procussion control-snares
;;;;
;;;; Creates main instrument and two sub-instruments
;;;;
;;;;     control-snares
;;;;        ctrlsnr-1
;;;;        ctrlsnr-2
;;;; 
;;;; The main instrument has the combined keynumber maps of the two
;;;; sub-instruments.
;;;;
;;;; Zone Stack
;;;;  1 153 GarbageBoy    : 036 041                                                               
;;;;  2 148 houseSnr 6    : 041 046                                                            
;;;;  3 092 Stereo Snr    : 046 051                                                           
;;;;  4 159 BrushDance    : 051 056                                                             
;;;;  5 112 WetSnare 3    : 056 061                            
;;;;  6 163 TamSnare 2    : 061 066                                                            
;;;;  7 104 Slap Snare    : 066 070                                                         
;;;;  8                   :                                                                 
;;;;  9                   :                                                                
;;;; 10                   :                                                                      
;;;; 11                   :                                                                    
;;;; 12                   :                                                                    
;;;; 13 129 HerboSnare    : 071 073                                                             
;;;; 14 168 BckwrdSnr     : 074 076                               
;;;; 15 118 ModVerbSnr    : 077 079                                                         
;;;; 16 172 RevSnare 3    : 080 082                                  
;;;; 17 100 TonalSnare    : 083 085                                               
;;;; 18 115 Amni-Snr 1    : 086 088                                               
;;;; 19 132 SnareSmash    : 089 091                                              
;;;; 20 120 ModPan Snr    : 092 094                                      
;;;; 21 119 ModVerbRim    : 095 097                                      
;;;; 22 121 ReverbRim     : 098 100                                      
;;;; 23                   :                                              
;;;; 24 129 HerboSnare    : 104 106 
;;;;

(in-package :cyco)

(let*((program-number (car (cdr (assoc 'control-snares +PROCUSSION-PROGRAMS+))))
      ;; A garbage : 36 37 38 39 40
      ;; B house   : 41 42 43 44 45
      ;; C stereo  : 46 47 48 49 50
      ;; D brush   : 51 52 53 54 55
      ;; E wet     : 56 57 58 59 60
      ;; F tam     : 61 62 63 64 65
      ;; G slap    : 66 67 68 69 70
      (acc '())
      ;; H HerboSnare  : 71 72 73
      ;; I BckwrdSnr   : 74 75 76                              
      ;; J ModVerbSnr  : 77 78 79                                                        
      ;; K RevSnare 3  : 80 81 82                                 
      ;; L TonalSnare  : 83 84 85                                              
      ;; M Amni-Snr 1  : 86 87 88                                              
      ;; N SnareSmash  : 89 90 91                                             
      ;; O ModPan Snr  : 92 93 94                                     
      ;; P ModVerbRim  : 95 96 97                                     
      ;; Q ReverbRim   : 98 99 100                                    
      ;; R HerboSnare  : 104 105 106
      (bcc '()))
  (dotimes (counter 5)
    (dolist (spec '((A 36 "garbage")
		    (B 41 "house")
		    (C 46 "stereo")
		    (D 51 "brush")
		    (E 56 "wet")
		    (F 61 "tam")
		    (G 66 "slap")))
      (let ((name (if (zerop counter)
		      (car spec)
		    (intern (str+ (car spec) counter))))
	    (value (+ counter (car (cdr spec)))))
	(push (list name (cdr spec)) acc))))
  (setf acc (reverse acc))
	  
  (dotimes (counter 3)
    (dolist (spec '((H 71 "HerboSnare")
		    (I 74 "BckwrdSnr")
		    (J 77 "ModVerbSnr")
		    (K 80 "RevSnare")
		    (L 83 "TonalSnare")
		    (M 86 "Amni")
		    (N 89 "SnareSmash")
		    (O 92 "ModPan")
		    (P 95 "ModVerbRim")
		    (Q 98 "ReverbRim")
		    (R 104 "HerboSnare")))
      (let ((name (if (zerop counter)
		      (car spec)
		    (intern (str+ (car spec) counter))))
	    (value (+ counter (car (cdr spec)))))
	(push (list name (cdr spec)) bcc))))
  (setf bcc (reverse bcc))
  
  
  (param control-snares nil)
  (defun control-snares (&key (parent pro3))
    (setf control-snares (create-instrument 'control-snares
					  :parent parent
					  :transient t
					  :program-change-hook
					  (constant-program-hook
					   'control-snares program-number)
					  :keynumber-map (keymap 'control-snares
								 (append acc bcc))))
    (param ctrlsnr-1 (create-instrument 'ctrlsnr-1
					:parent control-snares
					:keynumber-map (keymap 'ctrlsnr-1 acc)))
    (param ctrlsnr2 (create-instrument 'ctrlsnr-2
				       :parent control-snares
				       :keynumber-map (keymap 'ctrlsnr-2 bcc)))
    control-snares)) 
