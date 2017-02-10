;;;; cyco/Local/Emu/Procussion/sound-fx.lisp
;;;; 2012.09.21
;;;; Instruments
;;;;     sound-fx
;;;;         ufo
;;;;         derAufish
;;;;         qoolKlanq
;;;;         noiseFx
;;;;         klicker
;;;;         noiseFx
;;;;         cheering
;;;;         starTree
;;;;         pipeline
;;;;         mutantMal
;;;;         voxFreak
;;;;
;;;; 01 :521 GongAttack  : 036 038
;;;; 02 :522 UFOs        : 039 041
;;;; 03 :523 Cheering    : 042 044
;;;; 04 :524 GneratoX    : 045 047
;;;; 05 :525 Noise FX    : 048 050
;;;; 06 :530 Rocket #1   : 051 053 *
;;;; 07 :531 Rocket #2   : 051 053 *
;;;; 08 :478 Sufin-USA   : 054 056
;;;; 09 :441 Star Tree   : 057 059
;;;; 10 :548 Klicker     : 060 062
;;;; 11 :468 MutntMal2   : 063 065
;;;; 12 :549 Der Aufish  : 066 068
;;;; 13 :550 Bubl Trubl  : 069 071
;;;; 14 :462 Pipeline    : 072 074
;;;; 15 :470 Big Pipe    : 075 077
;;;; 16 :56 Qool KlanQ   : 078 080
;;;; 17 :474 TinkleTine  : 081 083
;;;; 18 :551 FunkinNoys  : 084 086
;;;; 19 :554 Rasparity   : 087 089
;;;; 20 :552 Lazer Slew  : 090 092
;;;; 21 :450 VoxFreak 2  : 093 095
;;;; 22 :467 MutantMal1  : 096 098 
;;;; 23 :553 Kloggers    : 099 101
;;;; 24 :449 VoxFreak 1  : 102 104
;;;;

(defkmap --sfx-AttackRocket-map       '(036 051 037 052 038 053))
(defkmap --sfx-UFO-map                '(039 045 090 040 046 091 041 047 092))
(defkmap --sfx-DerAufish-map          '(066 069 067 070 068 071))
(defkmap --sfx-QoolKlanQ-map          '(078 087 079 088 080 089))
(defkmap --sfx-NoiseFX-map            '(048 084 049 085 050 086))
(defkmap --sfx-Klicker-map            '(060 099 061 100 062 101))
(defkmap --sfx-Cheering-map           '(042 054 043 055 044 056))
(defkmap --sfx-StarTree-map           '(057 081 058 082 059 083))
(defkmap --sfx-Pipeline-map           '(072 075 073 076 074 077))
(defkmap --sfx-MutantMal-map          '(096 063 097 064 098 065))
(defkmap --sfx-VoxFreak-map           '(102 093 103 094 104 095))

(param sound-fx nil)

(defun sound-fx (&optional (parent pro3))
  (remove-children parent)
  (set-value parent :program (procussion-program-map 'sound-fx))
  (setf sound-fx (instrument :sound-fx 
			     :parent parent))
  (param AttackRocket (instrument :AttackRocket
				  :parent sound-fx
				  :keymap #'--sfx-attackrocket-map))
  (param UFO (instrument :UFO
			 :parent sound-fx
			 :keymap #'--sfx-UFO-map))
  (param DerAufish (instrument :DerAufish
			       :parent sound-fx
			       :keymap #'--sfx-DerAufish-map))
  (param QoolKlanQ (instrument :QoolKlanQ
			       :parent sound-fx
			       :keymap #'--sfx-QoolKlanQ-map))
  (param NoiseFX (instrument :NoiseFX
			     :parent sound-fx
			     :keymap #'--sfx-NoiseFX-map))
  (param Klicker (instrument :Klicker
			     :parent sound-fx
			     :keymap #'--sfx-Klicker-map))
  (param Cheering (instrument :Cheering
			      :parent sound-fx
			      :keymap #'--sfx-Cheering-map))
  (param StarTree (instrument :StarTree
			      :parent sound-fx
			      :keymap #'--sfx-StarTree-map))
  (param Pipeline (instrument :Pipeline
			      :parent sound-fx
			      :keymap #'--sfx-Pipeline-map))
  (param MutantMal (instrument :MutantMal
			       :parent sound-fx
			       :keymap #'--sfx-MutantMal-map))
  (param VoxFreak (instrument :VoxFreak
			      :parent sound-fx
			      :keymap #'--sfx-VoxFreak-map))
  sound-fx)
