;;;; emu procussion sound-fx
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

(param sound-fx nil)

(let ((attack-rocket-map (circular-keymap 'attack-rocket '(036 051 037 052 038 053)))
      (ufo-map (circular-keymap 'ufo  '(039 045 090 040 046 091 041 047 092)))
      (derfish-map (circular-keymap 'derfish  '(066 069 067 070 068 071)))
      (qoolklanq-map (circular-keymap 'qoolklanq  '(078 087 079 088 080 089)))
      (noisefx-map (circular-keymap 'noisefx  '(048 084 049 085 050 086)))
      (klicker-map (circular-keymap 'klicker  '(060 099 061 100 062 101)))
      (cheer-map (circular-keymap 'cheer '(042 054 043 055 044 056)))
      (startree-map (circular-keymap 'startree '(057 081 058 082 059 083)))
      (pipeline-map (circular-keymap 'pipeline '(072 075 073 076 074 077)))
      (mutantmal-map (circular-keymap 'mutantmal  '(096 063 097 064 098 065)))
      (voxfreak-map (circular-keymap 'voxfreak '(102 093 103 094 104 095)))
      (program-number (car (cdr (assoc 'latin-drums +PROCUSSION-PROGRAMS+)))) )
  (setf sound-fx nil)

  (defun sound-fx (&key (parent pro3))
    (setf sound-fx (create-instrument 'sound-fx
				      :parent parent
				      :transient t
				      :program-change-hook
				      (constant-program-hook
				       'sound-fx program-number)))
    
    (param attack-rocket (create-instrument 'attack-rocket
					    :parent sound-fx
					    :keynumber-map attack-rocket-map))
    (param ufo (create-instrument 'ufo
				  :parent sound-fx
				  :keynumber-map ufo-map))
    (param derfish (create-instrument 'derfish
				      :parent sound-fx
				      :keynumber-map derfish-map))
    (param qoolklanq (create-instrument 'qoolklanq
					:parent sound-fx
					:keynumber-map qoolklanq-map))
    (param noisefx (create-instrument 'noisefx
				      :parent sound-fx
				      :keynumber-map noisefx-map))
    (param klicker (create-instrument 'klicker
				      :parent sound-fx
				      :keynumber-map klicker-map))
    (param cheer (create-instrument 'cheer
				    :parent sound-fx
				    :keynumber-map cheer-map))
    (param startree (create-instrument 'startree
				       :parent sound-fx
				       :keynumber-map startree-map))
    (param pipeline (create-instrument 'pipeline
				       :parent sound-fx
				       :keynumber-map pipeline-map))
    (param mutantmal (create-instrument 'mutantmal
					:parent sound-fx
					:keynumber-map mutantmal-map))
    (param voxfreak (create-instrument 'voxfreak
				       :parent sound-fx
				       :keynumber-map voxfreak-map))
    sound-fx))

 
