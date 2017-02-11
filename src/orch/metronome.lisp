;;;; cyco.orch.metronome
;;;;
;;;; Defines specialized INSTRUMENT for use as metronome.
;;;; The metronome produces three sounds: beep, accent and phrase.
;;;; The phrase sound is produced at the start of each phrase repetition.
;;;; Accent is produced at the start of each bar.
;;;; beep is produced for all other metronome ticks.
;;;;

(defmacro --metronome-keymap (beep accent phrase)
  `#'(lambda (kn)
       (cond ((eq kn 'beep) (car ,beep))
	     ((eq kn 'accent) (car ,accent))
	     ((eq kn 'phrase) (car ,phrase))
	     ((eq kn :?)
	      (format t "Metronome-keymap~%")
	      (format t "  beep   -> ~A~%" ,beep)
	      (format t "  accent -> ~A~%" ,accent)
	      (format t "  phrase -> ~A~%" ,phrase)
	      (car ,beep))
	     (t (car ,beep)))))

(defmacro --metronome-amplitude-map (beep accent phrase)
  ` #'(lambda (amp)
	(let ((rs (cond ((eq amp 'beep) (amplitude (second ,beep)))
			((eq amp 'accent) (amplitude (second ,accent)))
			((eq amp 'phrase) (amplitude (second ,phrase)))
			(t (second ,beep)))))
	  (amplitude rs))))
   
(defun metronome-instrument (&key
			      (name 'metronome)
			      (parent *root-instrument*)
			      (transient t)
			      (channel nil)
			      (beep   '(100 ff))
			      (accent '(101 fff))
			      (phrase '(102 fff))
			      (beep-duration 0.1)
			      (program-change-hook nil)
			      (program-change-offset nil) ; time of event offset 
			      (program-number 0)
			      (program-bank 0))
  "Create metronome INSTRUMENT
   name - 
   parent - parent instrument
   transient - boolean
   channel - nil, 
   beep   - list (keynumber amplitude)
   accent - list (keynumber amplitude)
   phrase - list (keynumber amplitude)
   beep-duration - time in seconds
   program-change-hook 
   program-number - 
   program-bank -"
  (create-instrument name
		     :parent parent
		     :transient transient
		     :channel channel
		     :program-change-hook program-change-hook
		     :program-number program-number
		     :program-bank program-bank
		     :keynumber-map (--metronome-keymap beep accent phrase)
		     :amplitude-map (--metronome-amplitude-map beep accent phrase)
		     :duration-map #'(lambda (d) beep-duration)
		     :program-change-hook program-change-hook
		     :program-number program-number
		     :program-bank program-bank))

(setf *metronome-instrument* (metronome-instrument :channel 10))
