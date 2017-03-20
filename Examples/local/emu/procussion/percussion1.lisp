;;;; emu procussion percussion1
;;;; 2012.09.02
;;;; Instruments
;;;;    percussion1
;;;;        woodblock
;;;;        clave
;;;;        agogo
;;;;        triangle
;;;;        mambo
;;;;        campana
;;;;        chacha
;;;;        cabasa1
;;;;        guiro
;;;;        shakernet
;;;;
;;;; Zone    Stack             key-range  cyco-instrument
;;;; Z01     S306 TempleBlok   036 038    WoodBlock
;;;; Z02     S367 Wood Block   039 041    WoodBlock
;;;; Z03     S365 Clave        042 044    Clave
;;;; Z04     S361 Agogo Bell   045 047    Agogo
;;;; Z05     S362 Triangle     048 050    Triangle
;;;; Z06     S363 TringlMute   051 053    Trinagle
;;;; Z07     S383 Mambo Open   054 056    Mambo
;;;; Z08     S384 Mambo Clsd   057 059    Mambo
;;;; Z09     S385 Campana Op   060 062    Campana
;;;; Z10     S386 CampanaHel   063 065    Campana
;;;; Z11     S387 ChaCha Opn   066 068    ChaCha
;;;; Z12     S388 ChaCha Cls   069 071    ChaCha
;;;; Z13     S389 CabasaFrnt   072 074  * Cabasa1
;;;; Z14     S390 CabasaBak    075 077  * Cabasa1
;;;; Z15     S391 CabasaRoll   078 080  * Cabasa1
;;;; Z16     S393 Guiro Up     081 083  * Guiro
;;;; Z17     S394 ShakerNet    084 086    shakernet
;;;; Z18     S395 ShakerSnp    087 089    shakernet
;;;; Z19     S396 ShakerBack   090 092    shakernet
;;;; Z20     S397 Shakerfrnt   093 096    shakernet
;;;; Z21     S365 Clave        098 098    Clave
;;;; Z22     S365 Clave        098 098    Clave
;;;; Z23     S392 Guiro Down   100 100    Guiro
;;;; Z24     S424 VlctyGuiro   101 101    Guiro

(in-package :cyco)

(param percussion1 nil)

(let ((woodblock-map (keymap 'p1-woodblock '((A    36)
					     (A2   37)
					     (A3   38)
					     (B    39)
					     (B2   40)
					     (B3   41))))
      (clave-map (keymap 'p1-clave '((A    42)
				     (B    98)
				     (A1   43)
				     (A2   44))))
      (agogo-map (keymap 'p1-agogo '((A    45)
				     (A1   46)
				     (A2   47))))
      (triangle-map (keymap 'p1-triangle '((A       48)
					   (A2      49)
					   (A3      50)
					   (MUTE    51)
					   (MUTE2   52)
					   (MUTE3   53))))
      (mambo-map (keymap 'p1-mambo '((OP    54)
				     (OP2   55)
				     (OP3   56)
				     (CL    57)
				     (CL2   58)
				     (CL3   59))))
      (campana-map (keymap 'p1-campana '((OP    60)
					 (OP2   61)
					 (OP3   62)
					 (HL    63)
					 (HL2   64)
					 (HL3   65))))
      (chacha-map (keymap 'p1-chacha  '((OP    66)
					(OP2   67)
					(OP3   68)
					(CL    69)
					(CL2   70)
					(CL3   71))))
      (cabasa-map (keymap 'p1-cabasa '((FRNT    72)
				       (BACK    75)
				       (ROLL    78)
				       (FRNT2   73)
				       (BACK2   76)
				       (ROLL2   79)
				       (FRNT3   74)
				       (BACK3   77)
				       (ROLL3   80))))
      (guiro-map (keymap 'p1-guiro '((UP      81)
				     (DOWN    100)
				     (VLCTY   101)
				     (UP2     82)
				     (UP3     83))))
      (shakernet-map (keymap 'p1-shakernet '((A       84)
					     (SNAP    87)
					     (BACK    90)
					     (FRNT    93)
					     (A2      85)
					     (SNAP2   88)
					     (BACK2   91)
					     (FRNT2   94)
					     (A3      86)
					     (SNAP3   89)
					     (BACK3   92)
					     (FRNT3   95))))
      (program-number (car (cdr (assoc 'percussion1 +PROCUSSION-PROGRAMS+)))))
  (setf percussion1 nil)
  
  (defun percussion1 (&key (parent pro3))
    (setf percussion1 (create-instrument
		       'percussion1
		       :parent parent
		       :transient t
		       :program-change-hook (constant-program-hook
					     'percussion1 program-number)))
    (param p1-woodblock (create-instrument
			 'woodblock
			 :parent percussion1
			 :keynumber-map woodblock-map))
    (param p1-clave (create-instrument
		     'clave
		     :parent percussion1
		     :keynumber-map clave-map))
    (param p1-agogo (create-instrument
		     'agogo
		     :parent percussion1
		     :keynumber-map agogo-map))
    (param p1-triangle (create-instrument
			'triangle
			:parent percussion1
			:keynumber-map triangle-map))
    (param p1-mambo (create-instrument
		     'mambo
		     :parent percussion1
		     :keynumber-map mambo-map))
    (param p1-campana (create-instrument
		       'campana
		       :parent percussion1
		       :keynumber-map campana-map))
    (param p1-chacha (create-instrument
		      'chacha
		      :parent percussion1
		      :keynumber-map chacha-map))
    (param p1-cabasa (create-instrument
		      'cabasa
		      :parent percussion1
		      :keynumber-map cabasa-map))
    (param p1-guiro (create-instrument
		     'guiro
		     :parent percussion1
		     :keynumber-map guiro-map))
    (param p1-shakernet (create-instrument
			 'shakernet
			 :parent percussion1
			 :keynumber-map shakernet-map))
    percussion1)) 
