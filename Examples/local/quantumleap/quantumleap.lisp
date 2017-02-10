;;;; cyco local quantumleap/quantumleap
;;;;

(in-package :CYCO)

(param quantumleap (create-instrument
		    'quantumleap
		    :parent *root-instrument*
		    :transient nil))

(param QL1 (create-instrument
	    'QL1
	    :parent quantumleap
	    :transient nil
	    :channel :QL1))

(param QL2 (create-instrument
	    'QL2
	    :parent quantumleap
	    :transient nil
	    :channel :QL2))
	    
(param QL3 (create-instrument
	    'QL3
	    :parent quantumleap
	    :transient nil
	    :channel :QL3))

(param QL4 (create-instrument
	    'QL4
	    :parent quantumleap
	    :transient nil
	    :channel :QL4))

(param QL5 (create-instrument
	    'QL5
	    :parent quantumleap
	    :transient nil
	    :channel :QL5))


(load-local "quantumleap/mor2-drums")
(load-local "quantumleap/mor2-bass")
(load-local "quantumleap/mor2-guitar")
(load-local "quantumleap/vop")
(load-local "quantumleap/gypsy")
(load-local "quantumleap/gypsy-accordians")
(load-local "quantumleap/gypsy-guitars")


