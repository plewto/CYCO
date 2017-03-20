;;;; cyco/Local/Emu/Procussion/beatbox.lisp
;;;; 2012.09.26
;;;; Instruments:
;;;;    beatbox
;;;;       bb-kick
;;;;       bb-snare
;;;;       bb-ido
;;;;       bb-tom
;;;;       bb-hat
;;;;       bb-tuned
;;;;
;;;;
;;;; By Zone
;;;;
;;;; 01 :047 Rap Kick      :036 036:-01
;;;; 02 :381 PlasticCow    :046 046:+00
;;;; 03 :141 Analog Snr    :038 038:+00
;;;; 04 :406 QuintoTone    :058 059:+15
;;;; 05 :380 Synclaves     :037 037:+01
;;;; 06 :347 Klapz         :039 039:+00
;;;; 07 :256 SynthHat B    :042 042:+01
;;;; 08 :255 SynthHat A    :044 044:+01
;;;; 09 :210 AnalogToms    :060 096:+23
;;;; 10 :255 SynthHat A    :050 052:-01
;;;; 11 :210 AnalogToms    :043 043:+17
;;;; 12 :210 AnalogToms    :045 045:+24
;;;; 13 :210 AnalogToms    :047 047:+34
;;;; 14 :210 AnalogToms    :040 040:+06
;;;; 15 :210 AnalogToms    :041 041:+11
;;;; 16 :210 AnalogToms    :057 057:+19
;;;; 17 :389 CabasaFrnt    :048 048:+09
;;;; 18 :390 CabasaBack    :049 049:+00
;;;; 19 :088 SnapSnare6    :053 056:+32
;;;; 20 :                  :xxx xxx:+00
;;;; 21 :                  :xxx xxx:+00
;;;; 22 :                  :xxx xxx:+00
;;;; 23 :110 WetSnare 1    :034 034:+00
;;;; 24 :                  :xxx xxx:+00
;;;;                  
;;;; By Keynum
;;;;
;;;; 23 :110 WetSnare 1    :034 034:+00  SNR
;;;; 01 :047 Rap Kick      :036 036:-01  KICK
;;;; 05 :380 Synclaves     :037 037:+01  IDO
;;;; 03 :141 Analog Snr    :038 038:+00  SNR 
;;;; 06 :347 Klapz         :039 039:+00  SNR
;;;;
;;;; 14 :210 AnalogToms    :040 040:+06  TOM
;;;; 15 :210 AnalogToms    :041 041:+11  TOM
;;;; 07 :256 SynthHat B    :042 042:+01  HAT
;;;; 11 :210 AnalogToms    :043 043:+17  TOM
;;;; 08 :255 SynthHat A    :044 044:+01  HAT
;;;; 12 :210 AnalogToms    :045 045:+24  TOM
;;;; 02 :381 PlasticCow    :046 046:+00  IDO
;;;; 13 :210 AnalogToms    :047 047:+34  TOM
;;;; 17 :389 CabasaFrnt    :048 048:+09  IDO
;;;; 18 :390 CabasaBack    :049 049:+00  IDO
;;;;
;;;; 10 :255 SynthHat A    :050 052:-01  HAT
;;;; 19 :088 SnapSnare6    :053 056:+32  SNR
;;;; 16 :210 AnalogToms    :057 057:+19  IDO
;;;; 04 :406 QuintoTone    :058 059:+15  TOM
;;;;
;;;; 09 :210 AnalogToms    :060 096:+23  tuned toms
;;;;

(in-package :cyco)

(param beatbox nil)

(let ((kick-map (keymap 'bbox-kick '((X . 036))))
      (snare-map (keymap 'bbox-snamre '((WET    . 034)
					(ANALOG . 038)
					(KLAPZ  . 039)
					(SNAP   . 053)
					(SNAP2  . 054)
					(SNAP3  . 055)
					(SNAP4  . 056))))
      ;; (ido-map (keymap 'bbox-ido '((CLAVE   . 037)
      ;; 				   (COW     . 046)
      ;; 				   (CABASA  . 048)
      ;; 				   (CABASA2 . 049)
      ;; 				   (ANALOG  . 057))))
      (tom-map (circular-keymap 'bbox-tom '(40 41 43 45 47 58)))
      (hat-map (circular-keymap 'bbox-hat '(42 44 50 51 52)))
      (tuned-map (reduced-keymap 'bbox-tuned 60 96))
      (program-number (car (cdr (assoc 'beatbox +PROCUSSION-PROGRAMS+)))))
  (setf beatbox nil)

  (defun beatbox (&key (parent pro3))
    (setf beatbox (create-instrument 'beatbox
				     :parent parent
				     :transient t
				     :program-change-hook
				     (constant-program-hook
				      'beatbox program-number)))
    (param bb-kick (create-instrument 'bb-kick
				      :parent beatbox
				      :keynumber-map kick-map))
    (param bb-snare (create-instrument 'bb-snare
				       :parent beatbox
				       :keynumber-map snare-map))
    (param bb-tom (create-instrument 'bb-tom
				     :parent beatbox
				     :keynumber-map tom-map))
    (param bb-hat (create-instrument 'bb-hat
				     :parent beatbox
				     :keynumber-map hat-map))
    (param bb-tuned (create-instrument 'bb-tuned
				       :parent beatbox
				       :keynumber-map tuned-map))
    beatbox))
    
