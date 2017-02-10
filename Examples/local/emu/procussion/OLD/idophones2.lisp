;;;; cyco/Local/Emu/Procussion/idophones2.lisp
;;;; 2013.04.20
;;;; Instruments
;;;;    ido2
;;;;        ido2.cabasa
;;;;        ido2.net        (shaker net)
;;;;        ido2.guiro
;;;;        ido2.tambourine
;;;;        ido2.block      (temple wood log castentet...)
;;;;        ido2.clave
;;;;        ido2.shakers   (combined cabasa, net and tambourine)
;;;;        ido2.kalimba   (tuned)
;;;;
;;;; Zn Rang       Stack              Tune Fine   Group1      Group2  
;;;; 01 036 - 036  389 CabasaFrnt   -  ..   ..  - Cabasa      Shakers   :
;;;; 02 037 - 037  390 CabasaBck    -  ..   ..  - Cabasa      Shakers   :
;;;; 03 038 - 038  391 CabasaRoll   -  ..   ..  - Cabasa      Shakers   :
;;;; 04 039 - 039  391 CabasaRoll   -  ..   ..  - Cabasa      Shakers   :
;;;; 05 040 - 040  394 ShakerNet    -  ..   ..  - Net         Shakers   :
;;;; 06 041 - 041  395 ShakerSnap   -  ..   ..  - Net         Shakers   :
;;;; 07 042 - 042  396 ShakerSnap   -  ..   ..  - Net         Shakers   :
;;;; 08 044 - 045  392 GuiroDown    - +07   ..  - Guiro                 :
;;;; 09 046 - 047  393 GuiroUp      -  ..   ..  - Guiro                 :
;;;; 10 048 - 048  368 Tambourine   -  ..   ..  - Tam         Shakers   :
;;;; 11 049 - 049  368 Tambourine   -  ..  +18  - Tam         Shakers   :
;;;; 12 050 - 050  379 ShakaTam     -  ..   ..  - Tam         Shakers   :
;;;; 13 051 - 051  379 ShakaTam     -  ..   ..  - Tam         Shakers   :
;;;; 14 052 - 052  511 BabyBreath   -  ..   ..  - Net         Shakers   :
;;;; 15 043 - 043  397 ShakerFrnt   -  ..   ..  - Net         Shakers   :
;;;; 16 055 - 057  419 ShakeIt      -  ..   ..  - Net         Shakers   :
;;;; 17 058 - 058  360 TempleBlck   -  ..   ..  - Block                 :
;;;; 18 059 - 059  367 WoodBlock    -  ..   ..  - Block                 :
;;;; 19 060 - 060  xxx LogDrum      -  ..   ..  - Block                 :
;;;; 20 061 - 061  352 AnalogTick   -  ..   ..  - Block                 :
;;;; 21 062 - 062  366 Castenet     -  ..   ..  - Blcok                 :
;;;; 22 063 - 063  371 AmbiStick    -  ..   ..  - Block                 :
;;;; 23 064 - 125  455 Kalimba      -  ..   ..  - Kalimba               :
;;;; 24 126 - 127  365 Clave        -  ..   ..  - Clave                 :

(in-package cm)

(defmap --idophones2-cabasa-map '((FRNT   . 036)
				  (BACK   . 037)
				  (ROLL   . 038)
				  (ROOL2  . 039)))

(defmap --idophones2-net-map '((NET    . 040)
			       (SNAP   . 041)
			       (FRONT  . 043)
			       (SHAKE  . 055)
			       (BREATH . 052)
			       (SNAP2  . 042)
			       (SHAKE2 . 056)
			       (SHAKE3 . 057)))

(defmap --idophones2-guiro-map '((DOWN  . 044)
				 (UP    . 046)
				 (DOWN2 . 045)
				 (UP    . 047)))

(defmap --idophones2-tambourine-map '((TAM   . 048)
				      (TAM2  . 049)
				      (TAM3  . 050)
				      (TAM4  . 051)))

(defmap --idophones2-block-map '((TEMPLE   . 058)
				 (WOOD     . 059)
				 (LOG      . 060)
				 (TICK     . 061)
				 (CASTENET . 062)
				 (STICK    . 063)))

(defmap --idophones2-clave-map '((A  . 126)
				 (B  . 127)))


;; Combines Cabasa, Net &  Tambourine and 
(defmap --idophones2-shakers-map '((FRNT   . 036)
				   (BACK   . 037)
				   (ROLL   . 038)
				   (ROOL2  . 039)
				   (NET    . 040)
				   (SNAP   . 041)
				   (FRONT  . 043)
				   (SHAKE  . 055)
				   (BREATH . 052)
				   (SNAP2  . 042)
				   (SHAKE2 . 056)
				   (SHAKE3 . 057)
				   (TAM    . 048)
				   (TAM2   . 049)
				   (TAM3   . 050)
				   (TAM4   . 051)))

(defun --idophones2-kalimba-map (k)
  (cond ((eq k '?)
	 (format t "Kalimba keymap chromatic beween 72 and 125~%")
	 (format t "Keys less then 24 transposed up 6 octaves~%")
	 (format t "Keys less then 72 transposed up 4 octaves~%")
	 (format t "Keys gretarer then 125 transposed down 1 octave~%")
	 72)
	(t (let ((n (keynum k)))
	     (cond ((< n 24)
		    (+ n 60))
		   ((< n 72)
		    (+ n 48))
		   ((> n 125)
		    (- n 12))
		   (t n))))))

(param ido2 nil)

(defun idophones2 (&optional (parent pro3)(program 'idophones2))
  (remove-children parent)
  (set-value parent :program (procussion-program-map program))
  (let ((kinst (instrument 'idophones2 :parent parent)))
    (setf ido2 kinst)
    (param ido2.cabasa (instrument
			:ido2.cabasa
			:parent kinst
			:keymap #'--idophones2-cabasa-map))
    (param ido2.net (instrument
		     :ido2.net
		     :parent kinst
		     :keymap #'--idophones2-net-map))
    (param ido2.guiro (instrument
		       :ido2.guiro
		       :parent kinst
		       :keymap #'--idophones2-guiro-map))
    (param ido2.tambourine (instrument
			    :ido2.tambourine
			    :parent kinst
			    :keymap #'--idophones2-tambourine-map))
    (param ido2.block (instrument
		       :ido2.block
		       :parent kinst
		       :keymap #'--idophones2-block-map))
    (param ido2.clave (instrument
		       :ido2.clave
		       :parent kinst
		       :keymap #'--idophones2-clave-map))
    (param ido2.shakers (instrument
			 :ido2.shakers
			 :parent kinst
			 :keymap #'--idophones2-shakers-map))
    (param ido2.kalimba (instrument
			 :ido2.kalimba
			 :parent kinst
			 :keymap #'--idophones2-kalimba-map))
    ido2))
