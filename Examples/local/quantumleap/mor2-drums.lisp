;;;; cyco local quantumleap mor2-drums
;;;;
;;;;  m2-drums
;;;;     m2-kick
;;;;     m2-snare
;;;;     m2-tom
;;;;     m2-cow
;;;;     m2-hat
;;;;     m2-cym
;;;;     m2-ride
;;;;
;;;; Instruments may use a "light-load" keynumber map where selected
;;;; "full-load" keys are duplicated.
;;;;
;;;; The m2-ride instrument is a subset of m2-cym.
;;;;

(in-package :cyco)


(param m2-cow nil)
(param m2-cym nil)
(param m2-drums nil)
(param m2-hat nil)
(param m2-kick nil)
(param m2-ride nil)
(param m2-snare nil)
(param m2-tom nil)

(let* ((kick-map (keymap 'm2-kick '((A 35)
				    (B 36)
				    (C 59)
				    (D 60))))
       (kick-light-load-map (keymap 'm2-kick-light '((A 35)
						     (B 36)
						     (C 35)
						     (D 36))))
       (snare-map (keymap 'm2-snare 
			  '((X       38)
			    (RIM     37)
			    (CRACK   39)
			    (EDGE    40)
			    (BOUNCE  62)
			    (FLAM    63)
			    (ROLL    64)
			    (X2      86)
			    (RIM2    85)
			    (CRACK2  87)
			    (EDGE2   88)
			    (A       38 "Emu Compatibility -> X")
			    (B       40 "Emu Compatibility -> Edge")
			    (C       62 "Emu Compatibility -> Bounce")
			    (D       37 "Emu Compatibility -> Crack"))))
       (snare-light-load-map (keymap 'm2-snare 
				     '((X       38)
				       (RIM     37)
				       (CRACK   39)
				       (EDGE    40)
				       (BOUNCE  40)
				       (FLAM    38)
				       (ROLL    40)
				       (X2      38)
				       (RIM2    37)
				       (CRACK2  39)
				       (EDGE2   40)
				       (A       38 "Emu Compatibility -> X")
				       (B       40 "Emu Compatibility -> Edge")
				       (C       40 "Emu Compatibility -> Edge")
				       (D       37 "Emu Compatibility -> Rim"))))
       (tom-map (keymap 'm2-tom  '((A         41)
       				   (B         43)
       				   (C         45)
       				   (D         47)
       				   (E         48)
       				   (F         50)
       				   (A-FLAM    65)
       				   (B-FLAM    67)
       				   (C-FLAM    69)
       				   (D-FLAM    71)
       				   (E-FLAM    72)
       				   (F-FLAM    74)
       				   (A-BOUNCE  89)
       				   (B-BOUNCE  91)
       				   (C-BOUNCE  93)
       				   (D-BOUNCE  95)
       				   (E-BOUNCE  96)
       				   (F-BOUNCE  98))))
       (tom-light-load-map (keymap 'm2-tom-light '((A         41)
						   (B         43)
						   (C         45)
						   (D         47)
						   (E         48)
						   (F         50)
						   (A-FLAM    41)
						   (B-FLAM    43)
						   (C-FLAM    45)
						   (D-FLAM    47)
						   (E-FLAM    48)
						   (F-FLAM    50)
						   (A-BOUNCE  41)
						   (B-BOUNCE  43)
						   (C-BOUNCE  45)
						   (D-BOUNCE  47)
						   (E-BOUNCE  48)
						   (F-BOUNCE  50))))
       (cow-map (keymap 'm2-cow '((A 32)(B 31)(C 33))))
       (hat-map (keymap 'm2-hat  '((X       42 "closed")
				   (OP      44 "1/3 open")
				   (OPN     46 "2/3 open")
				   (OPEN    49 "full open")
				   (PED     34 "Pedal")
				   (EDGE    66 "closed")
				   (EOP     68 "1/3 open")
				   (EOPN    70 "2/3 open")
				   (EOPEN   73 "full open")
				   (BELL    90 "closed")
				   (BOP     92 "1/3 open")
				   (BOPN    94 "2/3 open")
				   (BOPEN   97 "full open")
				   (STOMP   34 "Emu compatibility")
				   (STOMP2  34 "Emu compatibility")
				   (x2      42 "Emu compatibility")
				   (op2     44 "Emu compatibility")
				   (opn2    46 "Emu compatibility")
				   (open2   49 "Emu compatibility"))))
       (hat-light-load-map (keymap 'm2-hat-light  '((X       42)
						    (OP      44)
						    (OPN     46)
						    (OPEN    49)
						    (PED     34)
						    (EDGE    42)
						    (EOP     44)
						    (EOPN    46)
						    (EOPEN   49)
						    (BELL    42)
						    (BOP     44)
						    (BOPN    46)
						    (BOPEN   49))))
       (cym-map (keymap 'm2-cym  '((RIDE      51)
				   (A         52)
				   (A-CHOKE   53)
				   (B         54)
				   (B-CHOKE   55)
				   (C         56)
				   (C-CHOKE   57)
				   (EDGE      75)
				   (A-MID     76)
				   (B-MID     78)
				   (BELL      99)
				   (BELL2    100))))
       (cym-light-load-map (keymap 'm2-cym-light  '((RIDE      51)
						    (A         52)
						    (A-CHOKE   53)
						    (B         54)
						    (B-CHOKE   55)
						    (C         56)
						    (C-CHOKE   57)
						    (EDGE      51)
						    (A-MID     52)
						    (B-MID     54)
						    (BELL      99)
						    (BELL2    100))))
       (ride-map (keymap 'm2-ride '((X      51 "Duplicate of m2-cym RIDE")
				    (edge   75 "Duplicate of m2-cym EDGE")
				    (bell   99 "Duplicate of m2-cym BELL")
				    (bell2 100 "Duplicate of m2-cym BELL2")))))
  (param m2-drums nil)
  (param m2-kick nil)
  (param m2-snare nil)
  (param m2-tom nil)
  (param m2-cow nil)
  (param m2-hat nil)
  (param m2-cym nil)
  (param m2-ride nil)

  (defun m2-drums (&key (parent ql5)(light-load nil))
    (setf m2-drums (create-instrument 'm2-drums
				      :parent parent
				      :transient t))
    (setf m2-kick (create-instrument 'm2-kick
				     :parent m2-drums
				     :keynumber-map 
				     (if light-load
					 kick-light-load-map
				       kick-map)))
    (setf m2-snare (create-instrument 'm2-snare
				      :parent m2-drums
				      :keynumber-map
				      (if light-load
					  snare-light-load-map
					snare-map)))
    (setf m2-tom (create-instrument 'm2-tom
				    :parent m2-drums
				    :keynumber-map
				    (if light-load
					tom-light-load-map
				      tom-map)))
    (setf m2-cow (create-instrument 'm2-cow
				    :parent m2-drums
				    :keynumber-map cow-map))
    (setf m2-hat (create-instrument 'm2-hat
				    :parent m2-drums
				    :keynumber-map
				    (if light-load
					hat-light-load-map
				      hat-map)))
    (setf m2-cym (create-instrument 'm2-cym
				    :parent m2-drums
				    :keynumber-map
				    (if light-load
					cym-light-load-map
				      cym-map)))
    (setf m2-ride (create-instrument 'm2-ride
				     :parent m2-drums
				     :keynumber-map
				     ride-map))
    m2-drums)) 
    
