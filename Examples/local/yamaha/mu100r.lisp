;;;; cyco local yamaha MU100R
;;;;
;;;; Looks like the MU100R uses both LSB and MSB for bank select.
;;;; 

(in-package :cyco)


(defun --mu100r-program-hook (time cindex program-number bank)
  (if (eq program-number :?)
      (progn 
	(format t "MU100R programs~%")
	(format t "    program  (1..128)~%")
	(format t "    bank  ?  (0..120 | SFX)~%")
	(format t "    Not too sure about bank select events... needs research.~%")
	nil)
    (list
     (cons (- time 0.01)(midi-control-change cindex 0 bank))
     (cons time (midi-program-change cindex program-number)))))

(param MU100R (create-instrument
	       'MU100R
	       :parent yamaha
	       :transient nil
	       :channel :MU100R
	       :program-change-hook #'--mu100r-program-hook
	       ))

(defmacro mu100r (name program bank &key
		       (channel nil)
		       (keynumber-map nil)
		       (duration-map nil)
		       (amplitude-map nil))
  `(param ,name (create-instrument ',name
				   :parent mu100r
				   :transient nil
				   :channel ,channel
				   :keynumber-map ,keynumber-map
				   :duration-map ,duration-map
				   :amplitude-map ,amplitude-map
				   :program-number ,program
				   :program-bank ,bank)))

		       
	       
