;;;; cyco local korg
;;;;

(in-package cyco)

(param korg (create-instrument 'korg
			       :parent *root-instrument*
			       :channel 1
			       :transient nil))

;;; ---------------------------------------------------------------------- 
;;;			       Micro sampler


(param microsampler (create-instrument 'sampler
				       :parent korg
				       :transient nil
				       :channel :sampler))

;;; ---------------------------------------------------------------------- 
;;;				    R3


(let ((bnk-alist '())
      (keys "ABCDEFGHIJKLMNOP"))
  (dotimes (i (length keys))
    (let ((kw (make-keyword (char keys i)))
	  (val (* 8 i)))
      (push (cons kw val) bnk-alist)))

  (defun --r3-program-hook (time cindex prognum bank)
    (let ((bnk-offset (or (cdr (assoc bank bnk-alist)) 0)))
      (cond ((eq prognum :?)
	     (format t "Korg R3~%")
	     (format t "   program-number (1..8)~%")
	     (format t "   bank (:A..:P)~%")
	     nil)
	    (t (list (cons time
			   (midi-program-change cindex
						(+ (1- prognum)
						   bnk-offset))))))))
  (param r3 (create-instrument
	     'r3
	     :parent korg
	     :transient nil
	     :channel :R3
	     :program-change-hook #'--r3-program-hook))
	   

  (defmacro r3 (name bank program &key
  		     (keynumber-map nil)
  		     (duration-map nil)
  		     (amplitude-map nil))
    `(param ,name (create-instrument ',name
  				     :parent r3
  				     :transient t
  				     :keynumber-map ,keynumber-map
  				     :duration-map ,duration-map
  				     :amplitude-map ,amplitude-map
  				     :program-change-offset -0.01
  				     :program-number ,program
  				     :program-bank ,bank))))
		  
  

					
						 

  
