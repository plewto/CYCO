;;;; cyco/Local/Emu/Procussion/industry.lisp
;;;; 2012.09.21
;;;; Instruments
;;;;    industry
;;;;       industry-noise
;;;;       industry-tone
;;;;
;;;; 01 :490 ToneDrum 2  : 000 127
;;;; 02 :491 Industry    : 036 036
;;;; 03 :492 MasterBlast : 060 060
;;;; 04 :493 IceMissile  : 072 072
;;;; 05 :494 Fire Biter  : 048 048
;;;; 06 :491 Industry    : 096 096
;;;; 07 :493 IceMissile  : 042 042
;;;; 08 :492 MasterBlast : 078 078
;;;; 09 :494 Fire Biter  : 090 090
;;;; 10 :495 Dunk        : 054 054
;;;; 11 :495 Dunk        : 084 084
;;;; 12 :490 ToneDrum 2  : 000 127
;;;; 13 
;;;; 14
;;;; 15
;;;; 16
;;;; 17
;;;; 18
;;;; 19
;;;; 20
;;;; 21
;;;; 22
;;;; 23
;;;; 24


(param industry nil)

(defkmap --industry-noise-map '(36 60 72 48 96 42 78 90 54 84))

(defun industry (&optional (parent pro4))
  (remove-children parent)
  (set-value parent :program (procussion-program-map 'industry))
  (setf industry (instrument :industry
			     :parent parent))
  (param industry-tone (instrument :industry-tone
				   :parent industry))
  (param industry-noise (instrument :industry-noise
				    :parent industry
				    :keymap #'--industry-noise-map))
  industry)
			     
