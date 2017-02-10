;;;; cyco/Local/Emi/Procussion/found-sound.lisp
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

(defkmap --found-pang-map       '(36 37 38 39 40 41 42 43 44))
(defkmap --found-noise-map      '(45 53 46 54 47 55 56 57))
(defkmap --found-thwak-map      '(48 49 50 51 52))
(defkmap --found-rasp-map       '(58 59 60 61 62))
(defkmap --found-hammer-map     '(63 64 65 66 67))
(defkmap --found-stack-map      '(68 69 70 71 72))
(defkmap --found-stilleto-map   '(73 78 74 79 75 80 76 81 77 82))
(defkmap --found-clank-map      '(83 93 84 94 85 95 86 96 87))
(defkmap --found-lazer-map      '(88 89 90 91 92))
(defkmap --found-cheering-map   '(98))

(param found-sound nil)

(defun found-sound (&optional (parent pro3))
  (remove-children parent)
  (set-value parent :program (procussion-program-map 'found-sound))
  (setf found-sound (instrument :found-sound :parent parent))
  (param found-pang (instrument :found-pang 
				:parent found-sound
				:keymap #'--found-pang-map))
  (param found-noise (instrument :found-noise
				 :parent found-sound
				 :keymap #'--found-noise-map))
  (param found-thwak (instrument :found-thwak
				 :parent found-sound
				 :keymap #'--found-thwak-map))
  (param found-rasp (instrument :found-rasp
				:parent found-sound
				:keymap #'--found-rasp-map))
  (param found-hammer (instrument :found-hammer
				  :parent found-sound
				  :keymap #'--found-hammer-map))
  (param found-stack (instrument :found-stack
				 :parent found-sound
				 :keymap #'--found-stack-map))
  (param found-stilleto (instrument :found-stilleto
				    :parent found-sound
				    :keymap #'--found-stilleto-map))
  (param found-clank (instrument :found-clank
				 :parent found-sound
				 :keymap #'--found-clank-map))
  (param found-lazer (instrument :found-lazer
				 :parent found-sound
				 :keymap #'--found-lazer-map))
  (param found-cheering (instrument :found-cheering
				    :parent found-sound
				    :keymap #'--found-cheering-map))
  found-sound)
