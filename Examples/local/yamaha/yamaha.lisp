;;;; cyco local yamaha
;;;;

(in-package :cyco)


(param yamaha (create-instrument 'YAMAHA
				 :transient nil))

(load-local "yamaha/tx816")
(load-local "yamaha/sy35")
(load-local "yamaha/mu100r")

