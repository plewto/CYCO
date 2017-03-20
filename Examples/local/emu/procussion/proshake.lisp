;;;; emu procussion proshake
;;;;
;;;;  proshake
;;;;     tambourine
;;;;     cabasa
;;;;     shaker
;;;;     guiro
;;;;     noisy
;;;;     jangler
;;;;     crack
;;;;     circle-noys
;;;;     castanet
;;;;     surfinusa
;;;;
;;;; Zone Stack
;;;;  1 368 Tambourine    : 024 026     
;;;;  2 379 Shakatam      : 027 029     
;;;;  3 274 NoiseHat A    : 030 032     
;;;;  4 276 NoiseHat B    : 033 035     
;;;;  5 254 Syn Hat 2     : 036 038     
;;;;  6 267 HouseHat 5    : 039 041     
;;;;  7 269 Tam Hat       : 042 044     
;;;;  8 266 Castanet      : 045 047     
;;;;  9 389 CabasaFnt     : 048 050     
;;;; 10 390 CabasaBack    : 051 053     
;;;; 11 391 CabasaRoll    : 054 056     
;;;; 12 392 Guiro Down    : 057 058     
;;;; 13 393 Guiro Up      : 059         
;;;; 14 394 ShakerNet     : 060 062     
;;;; 15 395 ShakerSnp     : 063 065     
;;;; 16 396 ShakerBack    : 066 068     
;;;; 17 397 ShakerFrnt    : 069 071     
;;;; 18 419 Shake it      : 072 074     
;;;; 19 424 VlctyGuiro    : 075 077     
;;;; 20 479 VlctySweep    : 078 080     
;;;; 21 499 Jangler       : 081 083     
;;;; 22 519 Crack         : 084 086     
;;;; 23 520 CircleNoys    : 087 095     
;;;; 24 478 Surfin USA    : 096 127     
;;;;

(in-package :cyco)
(param proshake nil)
(let* ((program-number (car (cdr (assoc 'proshake +PROCUSSION-PROGRAMS+))))
       (tam-map (keymap 'tambourine '((x      24 "Tambourine")
				      (shake  27 "Shakatam")
				      (hit    36 "Syn Hat 2")
				      (palm   39 "Househat 5")
				      (x2     25 "Tambourine")
				      (shake2 28 "Shakatam")
				      (hit2   37 "Syn Hat 2")
				      (palm2  40 "Househat 5")
				      (x3     26 "Tambourine")
				      (shake3 29 "Shakatam")
				      (hit3   38 "Syn Hat 2")
				      (palm3  41 "Househat 5"))))
       (cabasa-map (keymap 'cabasa '((front  48)
				     (back   51)
				     (roll   54)
				     (front2 49)
				     (back2  52)
				     (roll2  55)
				     (front3 50)
				     (back3  53)
				     (roll3  56))))
       (shaker-map (keymap 'shaker '((net     60)
				     (snap    63)
				     (back    66)
				     (front   69)
				     (shake   72)
				     (net2    61)
				     (snap2   64)
				     (back2   67)
				     (front2  70)
				     (shake2  73)
				     (net3    62)
				     (snap3   65)
				     (back3   68)
				     (front3  71)
				     (shake3  74))))
       (guiro-map (keymap 'guiro '((down      57)
				   (up        59)
				   (velocity  75)
				   (down2     58)
				   (velocity2 76)
				   (velocity3 77))))
       (noisy-map (keymap 'noisyhat '((A         30)
				      (B         33)
				      (TAM       42)
				      (VELOCITY  78)
				      (A2        31)
				      (B2        34)
				      (TAM2      43)
				      (VELOCITY2 79)
				      (A3        32)
				      (B3        35)
				      (TAM3      44)
				      (VELOCITY3 80))))
       (jangler-map (circular-keymap 'jangler (range 81 83)))
       (crack-map (circular-keymap 'crack (range 84 86)))
       (circle-noys-map (circular-keymap 'circle-noys (range 87 95)))
       (castanet-map (circular-keymap 'castanet (range 45 47)))
       (surfinusa-map (reduced-keymap 'surfinusa 96 127)))

  (defun proshake (&key (parent pro3))
    (setf proshake (create-instrument 'proshake
				      :parent parent
				      :transient t
				      :program-change-hook
				      (constant-program-hook
				       'proshake program-number)))
    (param tambourine (create-instrument 'tambourine
					 :parent proshake
					 :keynumber-map tam-map))
    (param cabasa (create-instrument 'cabasa
				     :parent proshake
				     :keynumber-map cabasa-map))
    (param shaker (create-instrument 'shaker
				     :parent proshake
				     :keynumber-map shaker-map))
    (param guiro (create-instrument 'guiro
				    :parent proshake
				    :keynumber-map guiro-map))
    (param noisy (create-instrument 'noisy
				    :parent proshake
				    :keynumber-map noisy-map))
    (param jangler (create-instrument 'jangler
				      :parent proshake
				      :keynumber-map jangler-map))
    (param crack (create-instrument 'crack
				    :parent proshake
				    :keynumber-map crack-map))
    (param circle-noys (create-instrument 'circle-noys
					  :parent proshake
					  :keynumber-map circle-noys-map))
    (param castanet (create-instrument 'castanet
				       :parent proshake
				       :keynumber-map castanet-map))
    (param surfinusa (create-instrument 'surfinusa
					:parent proshake
					:keynumber-map surfinusa-map))
    proshake))
  
