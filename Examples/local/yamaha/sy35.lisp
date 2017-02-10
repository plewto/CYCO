;;;; cyco local yamaha SY35
;;;;

(in-package :cyco)


(defun --sy35-program-hook (bank prog)
  #'(lambda (time cindex _1 _2)
      (let ((offset (* 8 (1- bank))))
	(list (cons time
		    (midi-program-change cindex (+ offset (1- prog))))))))

(param SY35 (create-instrument
	     'SY35
	     :parent yamaha
	     :transient nil
	     :channel :SY35))
	    

(defmacro sy35 (name bank program &key
		     (factory nil)
		     (channel nil)
		     (keynumber-map nil)
		     (duration-map nil)
		     (amplitude-map nil))
  `(param ,name (create-instrument 
		 ',name
		 :parent sy35
		 :remarks (if ,factory
			      "Use factory 'Preset' ROM"
			    "Use 'Internal' RAM")
		 :transient nil
		 :channel ,channel
		 :keynumber-map ,keynumber-map
		 :duration-map ,duration-map
		 :amplitude-map ,amplitude-map
		 :program-number ,program
		 :program-bank ,bank
		 :program-change-hook (--sy35-program-hook ,bank ,program))))


