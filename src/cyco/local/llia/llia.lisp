;;;; cyco local llia.llia
;;;;

(in-package :cyco)

(param llia (create-instrument
	     'llia
	     :parent *root-instrument*
	     :channel :llia
	     :transient nil))

(param llia2 (create-instrument
	      'llia2
	      :parent *root-instrument*
	      :channel :llia2
	      :transient nil))


(defmacro llia (name program &key
		     (parent llia)
		     (channel nil)
		     (keynumber-map nil)
		     (duration-map nil)
		     (amplitude-map nil))
  `(param ,name (create-instrument
		 ',name
		 :parent ,parent
		 :channel ,channel
		 :transient t
		 :keynumber-map ,keynumber-map
		 :duration-map ,duration-map
		 :amplitude-map ,amplitude-map
		 :program-number ,program)))
	     
