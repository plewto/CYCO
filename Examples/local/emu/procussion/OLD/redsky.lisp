;;;; cyco/Local/Emu/Procussion/redsky.lisp
;;;; 2013.04.22
;;;; Instruments
;;;;    redsky
;;;;        rs-redsky
;;;;        rs-surfin
;;;;        rs-cheering
;;;;        rs-industry
;;;;        rs-blaster
;;;;        rs-fire0biter
;;;;        rs-ufo
;;;;        rs-ice-missile
;;;;        rs-sfx
;;;;        rs-halloewll
;;;;        rs-jungle-night
;;;;        rs-mutantmal
;;;;        rs-silvoon
;;;;        rs-pit-o-doom
;;;;        rs-smashtom
;;;;        rs-rasp
;;;;        rs-hyper
;;;;
;;;; All zones rout to effects3 bus
;;;; Mod 1: key   -> pitch +055
;;;; Mod 2: CtrlA -> Start +096 (mod wheel)
;;;; ---------------------------------------------------------------
;;;; ZN: Stack            : Key        : Tune   : Pan : mod
;;;; 01: 478 Surfin       : 024-035    :        :     : 
;;;; 02: 492 Cheering     : 036-047    : -12    :     :
;;;; 03: 491 Industry     : 048-059    :        :     :
;;;; 04: 492 MasterBlaster: 060-071 !  : -24    : -6  : 12
;;;; 05: 492 MasterBlaster: 060-071 !  : -18    : +6  :  2
;;;; 06: 494 FireBiter    : 072-077 @  : -12    : +6  :
;;;; 07: 494 FireBiter    : 072-077 @  : -24    : -6  :
;;;; 08: 522 UFO's        : 078-083    : -12    :     :  2
;;;; 09: 493 IceMissile   : 084-089 $  :        : +5  : 12
;;;; 10: 493 IceMissile   : 084-089 $  : -06    : -5  : 1
;;;; 11: 311 SFX 1        : 090-095    : -12    :     : 1
;;;; 12: 440 Hallowell    : 096-099    :        :     : 1
;;;; 13: 453 Red Sky      : 100-103    : -12    :     : 1
;;;; 14: 454 Jungle Night : 104-107    : -21    :     :
;;;; 15: 467 MutantMal1   : 108-111 ^  : -24    :     : 12
;;;; 16: 483 SpaceStone   : 108-115 ^& : (NT)   :     : 12
;;;; 17: 483 Silvoon      : 111-115  & :        :     :
;;;; 18: 456 Pit O Doom   : 116-119 *  :        :     : 
;;;; 19: 446 Dust Pang    : 116-119 *  :        :     : 
;;;; 20: 194 PowerTom     : 120-123 (  :        :     :  2
;;;; 21: 314 SmashCymbl   : 120-123 (  :        :     :  
;;;; 22: 315 BrashCymbl   : 124-126 )  : -44    :     : 
;;;; 23: 359 Raspiness    : 124-126 )  : -22    :     : 12
;;;; 24: 318 Hypereal     : 012-023    :        :     : 1
;;;;

(defkmap --redsky-surfin-map       (range-between 024 036))
(defkmap --redsky-cheering-map     (range-between 036 048))
(defkmap --redsky-industry-map     (range-between 048 060))
(defkmap --redsky-blaster-map      (range-between 060 072))
(defkmap --redsky-fire-biter-map   (range-between 072 078))
(defkmap --redsky-ufo-map          (range-between 078 084))
(defkmap --redsky-ice-missile-map  (range-between 084 090))
(defkmap --redsky-sfx-map          (range-between 090 096))
(defkmap --redsky-hallowell-map    (range-between 096 100))
(defkmap --redsky-redsky-map       (range-between 100 104))
(defkmap --redsky-jungle-night-map (range-between 104 108))
(defkmap --redsky-mutantmal-map    (range-between 108 112))
(defkmap --redsky-silvoon-map      (range-between 111 116))
(defkmap --redsky-pit-o-doom-map   (range-between 116 120))
(defkmap --redsky-smashtom-map     (range-between 120 124))
(defkmap --redsky-rasp-map         (range-between 124 127))
(defkmap --redsky-hyper-map        (range-between 012 024))

(constant +REDSKY-MASTER-KEYMAP+
	 '((Hypereal        . 012)
	   (Surf            . 024)
	   (Cheer           . 036)
	   (Industry        . 048)
	   (Blaster         . 060)
	   (FireBiter       . 072)
	   (UFO             . 078)
	   (Missile         . 084)
	   (SFX             . 090)
	   (Hallowell       . 096)
	   (RedSky          . 100)
	   (Jungle          . 104)
	   (Mutant          . 108)
	   (Silvoon         . 111)
	   (PitODoom        . 116)
	   (Smash           . 120)
	   (Rasp            . 124)
	   (Hypereal2       . 018)
	   (Surf2           . 030)
	   (Cheer2          . 042)
	   (Industry2       . 048)
	   (Blaster2        . 054)
	   (FireBiter2      . 066)
	   (UFO2            . 080)
	   (Missile2        . 086)
	   (SFX2            . 093)
	   (Hallowell2      . 098)
	   (RedSky2         . 100)
	   (Jungle2         . 106)
	   (Mutant2         . 110)
	   (Silvoon2        . 112)
	   (PitODoom2       . 118)
	   (Smash2          . 122)
	   (Rasp2           . 126)))

(defun --redsky-map (k)
  (cond ((eq k 'r) +REST+)
	((eq k '?)
	 (format t "Redsky keymap~%")
	 (format t "  Numeric: 012 - 126~%")
	 (format t "  Symbolic:")
	 (let ((counter 0))
	   (dolist (p +REDSKY-MASTER-KEYMAP+)
	     (if (zerop counter)
		 (progn (format t "~%    ")
			(setf counter 6)))
	     (format t "~A " (car p))
	     (setf counter (1- counter)))
	 (format t "~%")
	 +REST+))
	((numberp k)(truncate k))
	(t (or (cdr (assoc k +REDSKY-MASTER-KEYMAP+)) +REST+))))

(param redsky nil)

(defun redsky (&optional (parent pro4))
  (remove-children parent)
  (set-value parent :program (procussion-program-map 'redsky))
  (let ((rs (instrument :redsky 
			:parent parent
			:keymap #'--redsky-map)))
    (setf redsky rs)
    (param rs-redsky (instrument :rs-redsky
				 :parent rs
				 :keymap #'--redsky-redsky-map))
    (param rs-surfin (instrument :rs-surfin
				 :parent rs
				 :keymap #'--redsky-surfin-map))
    (param rs-cheering (instrument :rs-cheering
				   :parent rs
				   :keymap #'--redsky-cheering-map))
    (param rs-industry (instrument :rs-industry
				   :parent rs
				   :keymap #'--redsky-industry-map))
    (param rs-blaster (instrument :rs-blaster
				  :parent rs
				  :keymap #'--redsky-blaster-map))
    (param rs-fire-biter (instrument :rs-fire-biter
				     :parent rs
				     :keymap #'--redsky-fire-biter-map))
    (param rs-ufo (instrument :rs-ufo
			      :parent rs
			      :keymap #'--redsky-ufo-map))
    (param rs-ice-missile (instrument :rs-ice-missile
				      :parent rs
				      :keymap #'--redsky-ice-missile-map))
    (param rs-sfx (instrument :rs-sfx
			      :parent rs
			      :keymap #'--redsky-sfx-map))
    (param rs-hallowell (instrument :rs-hallowell
				    :parent rs
				    :keymap #'--redsky-hallowell-map))
    (param rs-jungle-night (instrument :rs-jungle-night
				       :parent rs
				       :keymap #'--redsky-jungle-night-map))
    (param rs-mutantmal (instrument :rs-mutantmal
				    :parent rs
				    :keymap #'--redsky-mutantmal-map))
    (param rs-silvoon (instrument :rs-silvoon
				  :parent rs
				  :keymap #'--redsky-silvoon-map))
    (param rs-pit-o-doom (instrument :rs-pit-o-doom
				     :parent rs
				     :keymap #'--redsky-pit-o-doom-map))
    (param rs-smashtom (instrument :rs-smashtom
				   :parent rs
				   :keymap #'--redsky-smashtom-map))
    (param rs-rasp (instrument :rs-rasp
			       :parent rs
			       :keymap #'--redsky-rasp-map))
    (param rs-hyper (instrument :rs-hyper
				:parent rs
				:keymap #'--redsky-hyper-map))
    rs))
