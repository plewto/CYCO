;;;; emu procussion found-sounds
;;;; 2013.04.22
;;;; Instruments:
;;;;    found-sound
;;;;        found-pang
;;;;        found-noise
;;;;        found-rasp
;;;;        found-hammer
;;;;        found-stack
;;;;        found-stilleto
;;;;        found-clank
;;;;        found-lazer
;;;;        found-cheering
;;;;
;;;; 01 :446 dust pang   : 036 044 PANG
;;;; 04 :321 noise burst : 045 047 NOISE
;;;; 05 :322 bug thwak   : 048 052 THWAK
;;;; 06 :323 WhiteNoise  : 053 057 NOISE
;;;; 07 :324 Big Rasp    : 058 062 RASP
;;;; 08 :325 Big Hammer  : 063 067 HAMMER
;;;; 09 :326 MetalStack  : 068 072 STACK
;;;; 10 :327 Stilleto    : 073 077 STILLETO
;;;; 11 :329 pipe        : 078 082 STILLETO
;;;; 12 :330 Clank       : 083 087 CLANK
;;;; 13 :331 Lazer Hit   : 088 092 LAZER
;;;; 14 :332 SonicCrack  : 093 096 CLANK
;;;; 15 :323 Cheering    : 098 098

(in-package :cyco)


(let ((pang-map (circular-keymap 'fs-pang '(36 37 38 39 40 41 42 43 44)))
      (noise-map (circular-keymap 'fs-noise '(45 53 46 54 47 55 56 57)))
      (thwak-map (circular-keymap 'fs-thwak '(48 49 50 51 52)))
      (rasp-map (circular-keymap 'fs-rasp '(58 59 60 61 62)))
      (hammer-map (circular-keymap 'fs-hammer '(63 64 65 66 67)))
      (stack-map (circular-keymap 'fs-stack '(68 69 70 71 72)))
      (stilleto-map (circular-keymap 'fs-stilleto '(73 78 74 79 75 80 76 81 77 82)))
      (clank-map (circular-keymap 'fs-clank '(83 93 84 94 85 95 86 96 87)))
      (lazer-map (circular-keymap 'fs-lazer '(88 89 90 91 92)))
      (cheer-map (circular-keymap 'fs-cheer '(98)))
      (program-number (car (cdr (assoc 'found-sounds +PROCUSSION-PROGRAMS+)))))
  (param found-sounds nil)

  (defun found-sounds (&key (parent pro3))
    (setf found-sounds (create-instrument 'found-sounds
					  :parent parent
					  :transient t
					  :program-change-hook
					  (constant-program-hook 'found-sounds program-number)))
    (param fs-pang (create-instrument 'fs-pang
				      :parent found-sounds
				      :keynumber-map pang-map))
    (param fs-noise (create-instrument 'fs-noise
				       :parent found-sounds
				       :keynumber-map noise-map))
    (param fs-thwak (create-instrument 'fs-thwak
				       :parent found-sounds
				       :keynumber-map thwak-map))
    (param fs-rasp (create-instrument 'fs-rasp
				      :parent found-sounds
				      :keynumber-map rasp-map))
    (param fs-hammer (create-instrument 'fs-hammer
					:parent found-sounds
					:keynumber-map hammer-map))
    (param fs-stack (create-instrument 'fs-stack
				       :parent found-sounds
				       :keynumber-map stack-map))
    (param fs-stilleto (create-instrument 'fs-stilleto
					  :parent found-sounds
					  :keynumber-map stilleto-map))
    (param fs-clank (create-instrument 'fs-clank
				       :parent found-sounds
				       :keynumber-map clank-map))
    (param fs-lazer (create-instrument 'fs-lazer
				       :parent found-sounds
				       :keynumber-map lazer-map))
    (param fs-cheer (create-instrument 'fs-cheer
				       :parent found-sounds
				       :keynumber-map cheer-map))
    found-sounds)) 
