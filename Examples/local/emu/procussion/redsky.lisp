;;;; emu procussion redsky
;;;;
;;;;    redsky (may be used directly)
;;;;        rs-hyperal
;;;;        rs-surf
;;;;        rs-cheering
;;;;        rs-industry
;;;;        rs-blaster
;;;;        rs-biter
;;;;        rs-UFO
;;;;        rs-ICE
;;;;        rs-SFX
;;;;        rs-pit-o-doom
;;;;        rs-toms
;;;;        rs-brash
;;;;
;;;; Zone Stack
;;;;  1 478 Surfin'USA    : 024 035
;;;;  2 523 Cheering      : 036 047
;;;;  3 491 Industray     : 048 059
;;;;  4 492 MasterBlast   : 060 071
;;;;  5 492 MasterBlast   : 060 071
;;;;  6 494 Fire Biter    : 072 077
;;;;  7 494 Fire Biter    : 072 077
;;;;  8 522 UFOs          : 078 083
;;;;  9 493 IceMissile    : 084 089
;;;; 10 493 IceMissile    : 084 089
;;;; 11 311 SFX 1         : 090 095
;;;; 12 444 Hallowell     : 096 099
;;;; 13 453 Red Sky       : 100 103
;;;; 14 454 JungleNite    : 104 107
;;;; 15 467 MutntMall     : 108 111
;;;; 16 383 SpaceStone    : 108 115
;;;; 17 458 Silvervoon    : 112 115
;;;; 18 456 Pit o Doom    : 116 119
;;;; 19 446 Dust Pang     : 116 119
;;;; 20 194 Power Toms    : 120 123
;;;; 21 314 SmashCymbl    : 120 123
;;;; 22 315 BrashCymbl    : 124 126
;;;; 23 359 Raspiness     : 124 126
;;;; 24 318 Hypereal      : 012 023

(in-package :cyco)
(param redsky nil)
(let* ((program-number (car (cdr (assoc 'redsky +PROCUSSION-PROGRAMS+))))
       (hypereal-map (reduced-keymap 'hypereal 12 23))
       (surf-map (reduced-keymap 'surfin 24 35))
       (cheer-map (reduced-keymap 'cheering 36 47))
       (industry-map (reduced-keymap 'industry 48 59))
       (blaster-map (circular-keymap 'blaster (range 60 71)))
       (fire-biter-map (circular-keymap 'fire-biter (range 72 77)))
       (ufo-map (circular-keymap 'ufo (range 78 83)))
       (ice-map (circular-keymap 'ice-missile (range 84 89)))
       (sfx-map (circular-keymap 'sfx (range 90 95)))
       (redsky-map (keymap 'redsky 
			   (let ((acc '()))
			     (dotimes (counter 4)
			       (dolist (spec '((hallo 96 "Hallowell")
					       (redsky 100 "Redsky")
					       (jnight 104 "JungleNite")
					       (MutantMall 108 "MutantMall")
					       (Silver 112 "Silvervoon")))
				 (let ((name (if (zerop counter)
						 (car spec)
					       (intern (str+ (car spec) counter))))
				       (value (+ counter (second spec))))
				   (push (list name value (third spec)) acc))))
			     (reverse acc))))
       (pit-map (circular-keymap 'pit-o-doom (range 116 119)))
       (tom-map (circular-keymap 'power-toms (range 120 123)))
       (brash-map (circular-keymap 'brash (range 124 125))))

  (defun redsky (&key (parent pro3))
    (setf redsky (create-instrument 'redsky
				      :parent parent
				      :transient t
				      :program-change-hook
				      (constant-program-hook
				       'redsky program-number)
				      :keynumber-map redsky-map))
    (param rs-hypereal (create-instrument 'rs-hypereal
					  :parent redsky
					  :keynumber-map hypereal-map))
    (param rs-surf (create-instrument 'rs-surf
				      :parent redsky
				      :keynumber-map surf-map))
    (param rs-cheering (create-instrument 'rs-cheering
					  :parent redsky
					  :keynumber-map cheer-map))
    (param rs-industry (create-instrument 'rs-industry
					  :parent redsky
					  :keynumber-map industry-map))
    (param rs-blater (create-instrument 'rs-blaster
					:parent redsky
					:keynumber-map blaster-map))
    (param rs-biter (create-instrument 'rs-biter
				       :parent redsky
				       :keynumber-map fire-biter-map))
    (param rs-ufo (create-instrument 'rs-ufo
				     :parent redsky
				     :keynumber-map ufo-map))
    (param rs-ice (create-instrument 'rs-ice
				     :parent redsky
				     :keynumber-map ice-map))
    (param rs-sfx (create-instrument 'rs-sfx
				     :parent redsky
				     :keynumber-map sfx-map))
    (param rs-pit-o-doom (create-instrument 'rs-pit-o-doom
					    :parent redsky
					    :keynumber-map pit-map))
    (param rs-toms (create-instrument 'rs-toms
				      :parent redsky
				      :keynumber-map tom-map))
    (param rs-brash (create-instrument 'rs-brash
				       :parent redsky
				       :keynumber-map brash-map))
    redsky))
