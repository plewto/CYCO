;;;; cyco emu procussion thundadome
;;;;
;;;; Instruments
;;;;    thundadome
;;;;        goldengate
;;;;        hallowell
;;;;
;;;; 01	:476 GoldenGate    : 036 - 059                                       
;;;; 03	:442 thundadoom    : 036 - 036                                       
;;;; 21	:309 Gong-o-doom   : 042 - 042                                        
;;;; 04	:442 thundadoom    : 048 - 048                                       
;;;; 22	:309 Gong-o-doom   : 054 - 054                                        
;;;;
;;;; 02	:440 Hallowell     : 060 - 096                                      
;;;; 05	:442 thundadoom    : 060 - 060                                       
;;;; 06	:060 Tam Kick 2    : 061 - 061                                       
;;;; 07	:348 RapClav       : 063 - 063                                    
;;;; 08	:072 KettleDrum    : 066 - 066                                       
;;;; 09	:258 Elec-hat-2    : 068 - 068                                       
;;;; 10	:259 Elec-hat-3    : 070 - 070                                       
;;;; 11	:458 Silvoon       : 072 - 073                                     
;;;; 12	:308 FingerCymb    : 075 - 075                                       
;;;; 13	:222 SynTom        : 078 - 078                                   
;;;; 14	:385 Campana       : 080 - 080                                    
;;;; 15	:318 Hyper-real    : 082 - 082                                       
;;;; 24	:536 Qool-Klanq    : 084 - 084                                       
;;;; 16 :507 Gong-Pow      : 085 - 085                                     
;;;; 17	:509 CymbalDrum    : 087 - 087                                       
;;;; 18	:226 FallingTom    : 090 - 090                                       
;;;; 19	:463 Springz       : 092 - 092                                    
;;;; 20	:376 Carillon      : 094 - 094                                     
;;;; 23	:029 wet-kick-5    : 096 - 096                                       
;;;;
;  3         4         5         6         7         8         9         1 
;  01234567890123456789012345678901234567890123456789012345678901234567890123456789
;01      ************************
;02                              *************************************
;        *     *     *     *
;                                ** *  * * *  * *  * * * ** *  * * * *
;  01234567890123456789012345678901234567890123456789012345678901234567890123456789
;

(in-package :cyco)

(reduced-keymap --thundadome-goldengate-map 35 60)
(reduced-keymap --thundadome-hallowell-map 59 97)

(param thundadome nil)
(param goldengate nil)
(param hallowell nil)

(defun thundadome (&key (parent pro3))
  (let ((program-number (car (cdr (assoc 'thundadome +PROCUSSION-PROGRAMS+)))))
    (setf thundadome (create-instrument
		      'thundadome
		      :parent parent
		      :transient t
		      :program-change-hook (constant-program-hook
					    'thundadome program-number)))
    (setf goldengate (create-instrument
		      'goldengate
		      :parent thundadome
		      :keynumber-map #'--thundadome-goldengate-map))
    (setf hallowell (create-instrument
		     'hallowell
		      :parent thundadome
		      :keynumber-map #'--thundadome-hallowell-map))
    thundadome))
