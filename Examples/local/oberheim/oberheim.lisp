;;;; cyco local oberheim
;;;;

(in-package :cyco)

(param obx-1000 (create-instrument
		 'obx-1000
		 :parent *root-instrument*
		 :transient nil
		 :channel :OBX
		 :program-change-hook
		 #'(lambda (time cindex progn bank)
		     (cond ((eq progn :?)
			    (format t "Oberheim Matrix 1000~%")
			    (format t "    programs (0...99)~%")
			    (format t "    bank (0..9)~%")
			    (format t "Bank number of documentation only and~%")
			    (format t "does not alter generated events~%")
			    nil)
			   (t (cons time
				    (midi-program-change cindex prognum)))))))

(defmacro obx-1000 (name program &key
			 (bank nil)
			 (channel :OBX)
			 (keynumber-map nil)
			 (duration-map nil)
			 (amplitude-map nil))
  `(param ,name (create-instrument ',name
				   :parent obx-1000
				   :transient t
				   :channel ,channel
				   :keynumber-map ,keynumber-map
				   :duration-map ,duration-map
				   :amplitude-map ,amplitude-map
				   :program-number ,program)))
     

