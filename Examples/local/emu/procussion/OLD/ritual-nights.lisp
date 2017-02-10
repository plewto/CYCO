;;;; cyco/Local/Emu/Procussion/ritual-nights.lisp
;;;; 2013.04.22
;;;; Instruments
;;;;    ritual-nights
;;;;        rn-indian
;;;;        rn-log
;;;;
;;;; 01 :516 Indian Man : 036 - 060
;;;; 02 :508 RitualDrum : 036 - 060
;;;; 03 :461 GangaLog   : 061 - 096
;;;; 04 :377 LogDrum    : 061 - 096
;;;; 05 :               : xxx - xxx
;;;; 06 :               : xxx - xxx
;;;;
;;;; Effectively two instruments split at key 60
;;;;

(defun --ritual-nights-indian-map (k)
  (cond ((eq k 'r) 0)
	((eq k '?) (format t "036 - 060~%") 0)
	((and (numberp k)(>= k 36)(< k 61)) (truncate k))
	((and k (symbolp k))
	 (--ritual-nights-indian-map (keynum k)))
	(t 0)))

(defun --ritual-nights-log-map (k)
  (cond ((eq k 'r) 0)
	((eq k '?) (format t "061 - 096~%") 0)
	((and (numberp k)(>= k 61)(< k 97))(truncate k))
	((and k (symbolp k))
	 (--ritual-nights-log-map (keynum k)))
	(t 0)))

(param ritual-nights nil)

(defun ritual-nights (&optional (parent pro3))
  (remove-children parent)
  (set-value parent :program (procussion-program-map 'ritual-nights))		   
  (setf ritual-nights (instrument :ritual-nights
				  :parent parent))
  (param rn-indian (instrument :rn-indian
			       :parent ritual-nights
			       :keymap #'--ritual-nights-indian-map))
  (param rn-log (instrument :rn-log
			    :parent ritual-nights
			    :keymap #'--ritual-nights-log-map))
  ritual-nights)
