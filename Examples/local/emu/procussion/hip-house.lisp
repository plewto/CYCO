;;;; emu procussion hip-house
;;;; 2013.04.22
;;;; Instruiments:
;;;;    hip-house
;;;;        hh-kick
;;;;        hh-snare
;;;;        hh-hat
;;;;        hh-cow
;;;;        hh-noise
;;;;        hh-bongo1
;;;;        hh-bongo2
;;;;        hh-sawbas
;;;;
;;;; ZN : STACK             : KEY       : TUNE
;;;; 01 : 014 Dry Kick 6    : 036       :        : *kick 
;;;; 02 : 116 BakwardSnr1   : 047       :        : *revsnr              
;;;; 03 : 140 DanceSnare    : 038       :        : *snr                 
;;;; 04 : 151 TamboSnare    : 037       :        : *snr                 
;;;; 05 : 099 ModernSnare   : 040       :        : *snr
;;;; 06 : 167 BacwrdSnr2    : 048       :        : *revsnr              
;;;; 07 : 282 ServoHat1     : 042       :        : *hat                 
;;;; 08 : 283 ServoHat2     : 044       :        : *hat                 
;;;; 09 : 284 ServoHat3     : 046       :        : *hat ped                
;;;; 10 : 167 BckwrdSnr2    : 050       :        : *revsnr              
;;;; 11 : 373 Cowtombell    : 041       : -08    : *cow                    
;;;; 12 : 373 Cowtombell    : 043       :        : *cow                 
;;;; 13 : 373 Cowtombell    : 045       : +08    : *cow                    
;;;; 14 : 432 ModSawTone    : 072 - 096 :        : bass                
;;;; 15 : 453 Red Sky       : 049       :        : *noise
;;;; 16 : 454 JungleNite    : 051       :        : *noise
;;;; 17 : 169 BckwrdSnar4   : 052       :        : *revsnr              
;;;; 18 : 272 Horny hat     : 066 - 071 :        : *hat
;;;; 19 : 346 Wack Claps    : 039       :        : *snr              
;;;; 20 : 170 RevSnare 1    : 053       :        : *snr                 
;;;; 21 : 160 RimTinTin     : 054 - 056 :        : *snr
;;;; 22 : 422 BongoBoy      : 097 - 127 :        : *bongo1               
;;;; 23 : 337 ErsatScrch    : 057 - 059 : -12    : *noise
;;;; 24 : 422 BongoBoy      : 060 - 065 :        : *bongo2              
;;;;
;;;;     3         4         5         6         7         8         9         0         
;;;;     01234567890123456789012345678901234567890123456789012345678901234567890
;;;; 14                                            *************************
;;;; 18                                      ******
;;;; 21                          ***
;;;; 22                                                                     **** -->
;;;; 23                            ***
;;;; 24                                ******
;;;;     01234567890123456789012345678901234567890123456789012345678901234567890
;;;;     3         4         5         6         7         8         9         0         

(in-package :cyco)

(param hip-house nil)

(let ((kick-map (keymap 'hip-house-kick '((A 36))))
      (snare-map (keymap 'hip-house-snare  '((dance   038)
					    (tambo   037)
					    (mod     040)
					    (clap    039)
					    (rim1    053)
					    (rim2    054)
					    (rim3    055)
					    (rim4    056)
					    (bkwr1   047)
					    (bkwr2   048)
					    (bkwr3   050)
					    (bkwr4   052))))
      (hat-map (keymap 'hip-house-hat '((x       042)
				       (op      044)
				       (ped     046)
				       (horn1    066)
				       (horn2    067)
				       (horn3    068)
				       (horn4    069)
				       (horn5    070)
				       (horn6    071))))
      (cow-map (keymap 'hip-house-cow '((A        041)
				       (B        043)
				       (C        045))))
      (noise-map (keymap 'hip-house-noise '((red        049)
					    (night      051)
					    (scratch1   057)
					    (scratch2   058)
					    (scrstch3   059))))
      (bongo1-map (circular-keymap 'hip-house-bongo1 (range 97 127)))
      (bongo2-map (circular-keymap 'hip-house-bongo2 (range 60 65)))
      (scratch-map (circular-keymap 'hip-house-scratch '(57 58 59)))
      (bass-map (reduced-keymap 'hiphose-bass 72 96))
      (program-number (car (cdr (assoc 'latin-drums +PROCUSSION-PROGRAMS+)))) )

  (defun hip-house (&key (parent pro3))
    (setf hip-house (create-instrument 'hip-house
				       :parent parent
				       :transient t
				       :program-change-hook
				       (constant-program-hook
					'hip-house program-number)))
    (param hip-house-kick (create-instrument 'hip-house-kick
					     :parent hip-house
					     :keynumber-map kick-map))
    (param hip-house-snare (create-instrument 'hip-house-snare
					      :parent hip-house
					      :keynumber-map snare-map))
    (param hip-house-hat (create-instrument 'hip-house-hat
					    :parent hip-house
					    :keynumber-map hat-map))
    (param hip-house-cow (create-instrument 'hip-house-cow
					    :parent hip-house
					    :keynumber-map cow-map))
    (param hip-house-noise (create-instrument 'hip-house-noise
					     :parent hip-house
					     :keynumber-map noise-map))
    (param hip-house-bongo1 (create-instrument 'hip-house-bongo1
					       :parent hip-house
					       :keynumber-map bongo1-map))
    (param hip-house-bongo2 (create-instrument 'hip-house-bongo2
					       :parent hip-house
					       :keynumber-map bongo2-map))
    (param hip-house-scratch (create-instrument 'hip-house-scratch
						:parent hip-house
						:keynumber-map scratch-map))
    (param hip-house-bass (create-instrument 'hip-house-bass
					     :parent hip-house
					     :keynumber-map bass-map))
    hip-house))
    
