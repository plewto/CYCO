;;;; emu procussion percusion2
;;;; 2013.04.22
;;;; Instruments
;;;;    percussion2
;;;;        multidrum2
;;;;        shaker2
;;;;        cow2
;;;;        clave2
;;;;        guiro2
;;;;        amazon2
;;;;
;;;; Zone   Stack            Key range
;;;; 01     :418 SlideConga  :036 - 039   :  DRUM
;;;; 02     :368 Tambourine  :039 - 042   :  SHAKER TAM
;;;; 03     :381 PlasticCow  :042 - 045   :  COW PLASTIC
;;;; 04     :379 Shakatam    :045 - 048   :  SHAKER TAM
;;;; 05     :380 Synclaves   :048 - 051   :  CLAVE
;;;; 06     :350 Bckwrd Swp  :051 - 054   :  SHAKER NOISE
;;;; 07     :374 Stereo Mba  :054 - 057   :  COW MBA
;;;; 08     :419 Shake it    :057 - 060   :  SHAKER SHAKER
;;;; 09     :420 Coga Set    :060 - 063   :  DRUM CONGA
;;;; 10     :422 Bongo Boy   :063 - 066   :  DRUM BONGO
;;;; 11     :424 VlctyGuiro  :066 - 069   :  GUIRO
;;;; 12     :377 Log Drum    :069 - 072   :  DRUM LOG
;;;; 13     :370 Cow Block   :072 - 075   :  COW BLOCK
;;;; 14     :423 Tune Drum   :075 - 078   :  DRUM TUNE
;;;; 15     :438 StoneBone   :078 - 081   :  DRUM BONE 
;;;; 16     :444 Drumbourine :081 - 084   :  DRUM TAM
;;;; 17     :439 Amazon      :084 - 087   :
;;;; 18     :443 ShakerDrum  :087 - 090   :  DRUM SHAKE
;;;; 19     :465 ShakerBell  :090 - 093   :  COW BELL
;;;; 20     :457 ShakerDrm2  :093 - 096   :  DRUM SHAKE
;;;; 21     :365 Clave       :098 - 098   :  CLAVE
;;;; 22     :365 Clave       :098 - 098   :  CLAVE
;;;; 23     :392 Guiro Down  :100 - 100   :  GUIRO
;;;; 24     :424 VlctyGuiro  :101 - 101   :  GUIRO
;;;;
;;;;
;;;; DRUM:
;;;; conga  36 37 38 39
;;;; conga2 61 62 60 63
;;;; bongo  62 64 63 65
;;;; log    68 69 67 70
;;;; tune   76 77 74 78
;;;; bone   79 80 78 81
;;;; tam    82 83 80 84
;;;; shake  88 89 94 95 87 90 93 96
;;;;
;;;; SHAKER:
;;;; SHAKER 58 59 57 60
;;;; NOISE  52 53 51 54
;;;; TAM    40 46 41 47 39 42 45 48
;;;;
;;;; COW
;;;; PLASTIC 43 44 42 45
;;;; BLOCK   73 74 72 75
;;;; BELL    91 92 90 93
;;;; MBA     55 56 54 57
;;;;
;;;; CLAVE   98 49 59 48 51
;;;;
;;;; GUIRO    VLCT 101 100 67 68 66 69
;;;;
;;;; AMAZON  85 86 84 87

(param percussion2 nil)

(let ((drum-map (keymap 'drum '((conga     36)
				(conga2    61)
				(conga3    37)
				(conga4    62)
				(conga5    38)
				(conga6    39)
				(conga7    60)
				(conga8    63)
				(bongo     62)
				(bongo2    64)
				(bongo3    63)
				(bongo4    65)
				(log       68)
				(log2      69)
				(log3      67)
				(log4      70)
				(tune      76)
				(tune2     77)
				(tune3     74)
				(tune4     78)
				(bone      79)
				(bone2     80)
				(bone3     78)
				(bone4     81)
				(tam       82)
				(tam2      83)
				(tam3      80)
				(tan4      84)
				(shake     88)
				(shake2    89)
				(shake3    94)
				(shake4    95)
				(shake5    87)
				(shake6    90)
				(shake7    93)
				(shake8    96))))
      (shaker-map (keymap 'shaker '((shaker    43)
				    (shaker2   44)
				    (shaker3   42)
				    (shaker4   45)
				    (noise     52)
				    (noise2    53)
				    (noise3    51)
				    (noise4    54)
				    (tam       40)
				    (tam2      46)
				    (tam3      41)
				    (tam4      47)
				    (tam5      39)
				    (tam6      42)
				    (tam7      45)
				    (tam8      48))))
      (cow-map (keymap 'cow '((palstic     43)
			      (plastic2    44)
			      (plastic3    42)
			      (plastic4    45)
			      (blk         73)
			      (blk2        74)
			      (blk3        72)
			      (blk4        75)
			      (bell        91)
			      (bell2       92)
			      (bell3       90)
			      (bell4       93)
			      (mba         54)
			      (mba2        55)
			      (mba3        56)
			      (mba4        57))))
      (clave-map (keymap 'clave '((x    98)
				  (x2   49)
				  (x3   59)
				  (x4   48)
				  (x5   51))))
      (guiro-map (keymap 'guiro '((x    101)
				  (x2   102)
				  (x3   67)
				  (x4   68)
				  (x5   66)
				  (x6   69))))
      (amazon-map (keymap 'amazon '((x    85)
				    (x2   86)
				    (x3   84)
				    (x4   87))))
      (program-number 
       (car (cdr (assoc 'percussion2 +PROCUSSION-PROGRAMS+)))) )

  (defun percussion2 (&key (parent pro3))
    (setf percussion2 (create-instrument
		       'percussion2
		       :parent parent
		       :transient t
		       :program-change-hook (constant-program-hook
					     'percussion2 program-number)))
    (param p2-drum (create-instrument 'p2-drum 
				      :parent percussion2
				      :keynumber-map drum-map))
    (param p2-shaker (create-instrument 'p2-shaker 
					:parent percussion2
					:keynumber-map shaker-map))
    (param p2-cow (create-instrument 'p2-cow 
				     :parent percussion2
				     :keynumber-map cow-map))
    (param p2-clave (create-instrument 'p2-clave 
				       :parent percussion2
				       :keynumber-map clave-map))
    (param p2-guiro (create-instrument 'p2-guiro 
				       :parent percussion2
				       :keynumber-map guiro-map))
    (param p2-amazon (create-instrument 'p2-amazon 
					:parent percussion2
					:keynumber-map amazon-map))
    percussion2))
