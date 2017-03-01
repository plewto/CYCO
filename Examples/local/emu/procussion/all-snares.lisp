;;;; emu procussion all-snares
;;;;
;;;; Zone Stack
;;;;  1 130 KookySnare    : 036 041   36 37 38 39 | A                                             
;;;;  2 133 Pipe Snare    : 040 045   40 41 42 43 | B                                          
;;;;  3 084 SnapSnare2    : 044 049   44 45 46 47 | C                                         
;;;;  4 144 HouseSnr 2    : 048 053   48 49 50 51 | D                                           
;;;;  5 099 ModernSnare   : 052 057               | E          
;;;;  6 086 SnapSnare4    : 056 061               | F                                          
;;;;  7 101 bonzoSnare    : 060 065               | G                                       
;;;;  8 125 SNARE 1157    : 064 069               | H                                       
;;;;  9 131 ToughSnare    : 068 073               | I                                      
;;;; 10 092 Stereo Snr    : 072 077               | J                                            
;;;; 11 132 SnareSmash    : 076 081               | K                                          
;;;; 12 106 SnareCenter   : 080 085               | L                                          
;;;; 13 122 Rim Snare     : 084 089               | M                                           
;;;; 14 145 HouseSnr 3    : 088 093               | N             
;;;; 15 152 DeepTamSnare  : 091 096               | O                                       
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

(let*((program-number (car (cdr (assoc 'all-snares +PROCUSSION-PROGRAMS+))))
      (acc '()))
  (dotimes (counter 4)
    (dolist (spec '((A 036 "KookySnare")
		    (B 040 "Pipe Snare")
		    (C 044 "SnapSnare2")
		    (D 048 "HouseSnr 2")
		    (E 052 "ModernSnare")
		    (F 056 "SnapSnare4")
		    (G 060 "BonzoSnare")
		    (H 064 "SNARE 1157")
		    (I 068 "ToughSnare")
		    (J 072 "Stereo Snr")
		    (K 076 "SnareSmash")
		    (L 080 "SnareCenter")
		    (M 084 "Rim Snare")
		    (N 088 "HouseSnr 3")
		    (O 091 "DeepTamSnare")))
      (let ((name (if (zerop counter)
		      (car spec)
		    (intern (str+ (car spec) counter))))
	    (value (+ counter (second spec))))
	(push (list name value (third spec)) acc))))
  (param all-snares nil)
  (defun all-snares (&key (parent pro3))
    (setf all-snares (create-instrument 'all-snares
					 :parent parent
					 :transient t
					 :program-change-hook
					 (constant-program-hook
					  'all-snares program-number)
					 :keynumber-map
					 (keymap
					  'all-snares (reverse acc))))
    all-snares))
  
