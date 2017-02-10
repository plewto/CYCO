;;;; cyco/Local/Emu/Procussion/churchyard.lisp
;;;; 2013.04.22
;;;; Instruments:
;;;;    churchyard
;;;;        church-pipe
;;;;        church-carillon
;;;;        church-pang
;;;;        church-ride
;;;;        church-kick
;;;;
;;;; 01	:470 Big Pipe      : 036 - 059                                       
;;;; 02	:376 Carillon      : 060 - 096                                      
;;;; 03	:446 Dust pang     : 036 - 036                                       
;;;; 04	:446 Dust Pang     : 048 - 048                                       
;;;; 05	:446 Dust Pang     : 060 - 060
;;;; 06	:298 HypRideCym    : 061 - 061                                       
;;;; 07	:298 HypRideCym    : 063 - 063                                    
;;;; 08	:298 HypRideCym    : 066 - 066                                       
;;;; 09	:298 HypRideCym    : 068 - 068                                       
;;;; 10	:298 HypRideCym    : 070 - 070                                       
;;;; 11	:298 HypRideCym    : 072 - 073                                     
;;;; 12	:298 HypRideCym    : 075 - 075                                       
;;;; 13	:298 HypRideCym    : 078 - 078                                   
;;;; 14	:298 HypRideCym    : 080 - 080                                    
;;;; 15	:298 HypRideCym    : 082 - 082                                       
;;;; 16 :298 HypRideCym    : 085 - 085                                     
;;;; 17	:298 HypRideCym    : 087 - 087                                       
;;;; 18	:298 HypRideCym    : 090 - 090                                       
;;;; 19	:298 HypRideCym    : 092 - 092                                    
;;;; 20	:298 HypRideCym    : 094 - 094
;;;; 21	:536 Qool KlanQ    : 042 - 054                                        
;;;; 22	:041 Kick Space    : 072 - 072                                        
;;;; 23	:041 Kick Space    : 096 - 096                                       
;;;; 24	:041 Kick Space    : 084 - 084                                       

;
;       3         4         5         6         7         8         9         1 
;       01234567890123456789012345678901234567890123456789012345678901234567890123456789
;PIPE         ************************
;CARILLON                             *************************************
;QoolKlanq          ************
;DUSTPANG     *           *           *
;HYPRIDE                               * *  * * * ** *  * * *  * *  * * *
;KICKSPACE                                        *           *           *
;       01234567890123456789012345678901234567890123456789012345678901234567890123456789
;       3         4         5         6         7         8         9         1 

(in-package :cyco)         

(reduced-keymap --churchyard-pipe-map 36 59)
(reduced-keymap --churchyard-carillon-map 60 96)
(reduced-keymap --churchyard-klang-map 42 54)
(circular-keymap --churchyard-pang-map '(36 48 60))
(circular-keymap --churchyard-ride-map '(61 63 66 68 70 72 75 78
					80 82 85 86 90 92 94))
(circular-keymap --churchyard-kick-map '(72 84 96))

(param churchyard nil)
(param churchyard-pipe nil)
(param churchtard-klang nil)
(param churchtard-carillon nil)
(param churchtard-pang nil)
(param churchtard-ride nil)
(param churchtard-kick nil)

(defun churchyard (&key (parent pro3))
  (let ((program-number (car (cdr (assoc 'churchyard +PROCUSSION-PROGRAMS+)))))
    (setf churchyard (create-instrument
		      'churchyard
		      :parent parent
		      :transient t
		      :program-change-hook (constant-program-hook
					    'churchyard program-number)))
    (setf churchyard-pipe (create-instrument
			   'churchyard-pipe
			   :parent churchyard
			   :keynumber-map #'--churchyard-pipe-map))
    (setf churchyard-klang (create-instrument
			    'churchyard-klang
			    :parent churchyard
			    :keynumber-map #'--churchyard-klang-map))
    (setf churchyard-carillon (create-instrument
			       'churchyard-carillon
			       :parent churchyard
			       :keynumber-map #'--churchyard-carillon-map))
    (setf churchyard-pang (create-instrument
			   'churchyard-pang
			   :parent churchyard
			   :keynumber-map #'--churchyard-pang-map))
    (setf churchyard-ride (create-instrument
			   'churchyard-ride
			   :parent churchyard
			   :keynumber-map #'--churchyard-ride-map))
    (setf churchyard-kick (create-instrument
			   'churchyard-kick
			   :parent churchyard
			   :keynumber-map #'--churchyard-kick-map))
    churchyard))
