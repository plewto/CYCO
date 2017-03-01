;;;; cyco local yamaha/tx816
;;;;

(in-package :cyco)


(defun --tx-program-hook (program)
  #'(lambda (time cindex _1 _2)
      (dismiss _1 _2)
      (list (cons time
		  (midi-program-change cindex (1- program))))))

(param TX816 (create-instrument
	      'TX816
	      :parent yamaha
	      :transient nil
	      :program-change-hook
	      #'(lambda (time cindex program bank)
		  (dismiss bank)
		  (cond ((eq program :?)
			 (progn
			   (format t "TX816 program change hook")
			   nil))
			(t (list (cons time (midi-program-change
					     cindex (1- program))))) ))))

(param TXA (create-instrument 'TXA
			      :parent TX816
			      :transient nil
			      :channel :TXA))

(param TXB (create-instrument 'TXB
			      :parent TX816
			      :transient nil
			      :channel :TXB))

(param TXC (create-instrument 'TXC
			      :parent TX816
			      :transient nil
			      :channel :TXC))

(defmacro txa (name program &key
		    (keynumber-map nil)
		    (duration-map nil)
		    (amplitude-map nil))
  `(param ,name (create-instrument
		 ',name
		 :parent txa
		 :transient t
		 :keynumber-map ,keynumber-map
		 :duration-map ,duration-map
		 :amplitude-map ,amplitude-map
		 :program-number ,program
		 :program-bank 0
		 :program-change-hook (--tx-program-hook ,program))))

(defmacro txb (name program &key
		    (keynumber-map nil)
		    (duration-map nil)
		    (amplitude-map nil))
  `(param ,name (create-instrument
		 ',name
		 :parent txb
		 :transient t
		 :keynumber-map ,keynumber-map
		 :duration-map ,duration-map
		 :amplitude-map ,amplitude-map
		 :program-number ,program
		 :program-bank 0
		 :program-change-hook (--tx-program-hook ,program))))

(defmacro txc (name program &key
		    (keynumber-map nil)
		    (duration-map nil)
		    (amplitude-map nil))
  `(param ,name (create-instrument
		 ',name
		 :parent txc
		 :transient t
		 :keynumber-map ,keynumber-map
		 :duration-map ,duration-map
		 :amplitude-map ,amplitude-map
		 :program-number ,program
		 :program-bank 0
		 :program-change-hook (--tx-program-hook ,program))))

