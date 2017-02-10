;;;; Cyco/Local/Emu/Procussion/percussion1.lisp
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

(in-package :cm)

(defmap --percussion1-woodblock-map (zip 
				     '((A  . 36)
				       (A2 . 37)
				       (A3 . 38))
				     '((B  . 39)
				       (B2 . 40)
				       (B3 . 41))))

(defmap --percussion1-clave-map '((A  . 42)
				  (B  . 98)
				  (A1 . 43)
				  (A2 . 44)))

(defmap --percussion1-agogo-map '((A  . 45)
				  (A1 . 46)
				  (A2 . 47)))

(defmap --percussion1-triangle-map (zip 
				    '((A     . 48)
				      (A2    . 49)
				      (A3    . 50))
				    '((MUTE  . 51)
				      (MUTE2 . 52)
				      (MUTE3 . 53))))
			       
(defmap --percussion1-mambo-map (zip 
				 '((OP  . 54)
				   (OP2 . 55)
				   (OP3 . 56))
				 '((CL  . 57)
				   (CL2 . 58)
				   (CL3 . 59))))

(defmap --percussion1-campana-map (zip 
				   '((OP  . 60)
				     (OP2 . 61)
				     (OP3 . 62))
				   '((HL  . 63)
				     (HL2 . 64)
				     (HL3 . 65))))

(defmap --percussion1-chacha-map (zip 
				  '((OP  . 66)
				    (OP2 . 67)
				    (OP3 . 68))
				  '((CL  . 69)
				    (CL2 . 70)
				    (CL3 . 71))))

(defmap --percussion1-cabasa-map '((FRNT  . 72)
				   (BACK  . 75)
				   (ROLL  . 78)
				   (FRNT2 . 73)
				   (BACK2 . 76)
				   (ROLL2 . 79)
				   (FRNT3 . 74)
				   (BACK3 . 77)
				   (ROLL3 . 80)))

(defmap --percussion1-guiro-map '((UP    . 81)
				  (DOWN  . 100)
				  (VLCTY . 101)
				  (UP2   . 82)
				  (UP3   . 83)))

(defmap --percussion1-shakernet-map '((A     . 84)
				      (SNAP  . 87)
				      (BACK  . 90)
				      (FRNT  . 93)
				      (A2    . 85)
				      (SNAP2 . 88)
				      (BACK2 . 91)
				      (FRNT2 . 94)
				      (A3    . 86)
				      (SNAP3 . 89)
				      (BACK3 . 92)
				      (FRNT3 . 95)))
(param percussion1 nil)

(defun percussion1 (&optional (parent pro3))
  (remove-children parent)
  (set-value parent :program (procussion-program-map 'percussion1))
  (let ((kinst (instrument 'percussion1
			   :parent parent)))
    (param woodblock (instrument 
		      :woodblock
		      :parent kinst
		      :keymap #'--percussion1-woodblock-map))
    (param clave (instrument
		  :clave
		  :parent kinst
		  :keymap #'--percussion1-clave-map))
    (param agogo (instrument 
		  :agogo
		  :parent kinst
		  :keymap #'--percussion1-agogo-map))
    (param triangle (instrument
		     :triangle
		     :parent kinst
		     :keymap #'--percussion1-triangle-map))
    (param mambo (instrument
		  :mambo
		  :parent kinst
		  :keymap #'--percussion1-mambo-map))
    (param campana (instrument
		    :campana
		    :parent kinst
		    :keymap #'--percussion1-campana-map))
    (param chacha (instrument
		   :chacha
		   :parent kinst
		   :keymap #'--percussion1-chacha-map))
    (param cabasa1 (instrument
		    :cabasa1
		    :parent kinst
		    :keymap #'--percussion1-cabasa-map))
    (param guiro (instrument
		  :guiro
		  :parent kinst
		  :keymap #'--percussion1-guiro-map))
    (param shakernet (instrument
		      :shakernet
		      :parent kinst
		      :keymap #'--percussion1-shakernet-map))
    (setf percussion1 kinst)
    kinst))
