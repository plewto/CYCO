;;;; cyco local yamaha/tx816
;;;;

(in-package :cyco)


(constant +TXA-PROGRAMS+
	  '((BASS-A1    0 "BASS-1        Bass-Keys ")
	    (BASS-A2    1 "BASS-1        BASS1EDBOW")
	    (BASS-A3    2 "BASS-1        Best Bass ") 
	    (BASS-A4    3 "BASS-1        FrtLess II") 
	    (BASS-A5    4 "BASS-1        FrtLess IB") 
	    (BASS-A6    5 "BASS-1        FrtLess IC") 
	    (BASS-B1    6 "BASS-MOVIE    BASS THUMB") 
	    (BASS-C1    7 "BASS2-PULZ    BASS1EDBOW") 
	    (BASS-C2    8 "BASS2-PULZ    Best Bass ") 
	    (BASS-C3    9 "BASS2-PULZ    FrtLess II") 
	    (BASS-C4   10 "BASS2-PULZ    FrtLess IB") 
	    (FRTLSS-2  11 "FrtLess IB    FrtLess II") 
	    (FRTLSS-1B 12 "FrtLess IB    FrtLess IB") 
	    (SUBHARM   13 "SUBASS1       SB1Harmnc ") 
	    (BASSORCH  14 "FlatBass      5thFlat   ") 
	    (SIMPAD    15 "SIMPSYNTH1    ODDSYN    ") 
	    (INIVOICE  31 "INVOICE       INVOICE   ")))

(constant +TXB-PROGRAMS+
	  '((PIANO-21    1   "PIANO 21A   PIANO 21B ")
	    (PIANO-26    2   "PIANO 26A   PIANO 26B ")
	    (RHODES      3   "RHODESdb1   RHODESdb2 ")
	    (INIVOICE    4   "INIVOICE    INIVOICE  ")
	    (MBOX        5   "MBOX1       CELESTA1  ")
	    (GLOCKEN     6   "GLOCKEN  1  GLOCKEN  2")
	    (DIRT-PAD    7   "SIMPSYNTH2  DIRTAMB   ")
	    (MULTI-VIBE  8   "NOTYNE      MULTIVIBE ")
	    (HI-BELL     9   "HiBells01   HiBell02  ")
	    (TUBE-BELL  10   "TUB BELLS   SharpBells")
	    (INIVOICE   32   "INIVOICE    INIVOICE  ")))

(constant +TXC-PROGRAMS+
	  '((HI-PIANO      1   "MelPno01      HiPno01   ")
	    (PIANO-CP70    2   "CLEANPIANO    YAMAHACP70")
	    (PIANO-26      3   "PIANO 26A     PIANO 26B ")
	    (INIVOICE      4   "INIVOICE      INIVOICE  ")
	    (VIBES-1       5   "VIBE1A        VIBE1B    ")
	    (ICE9          6   "ICE9A         ICE9B     ")
	    (GLASSICE      7   "GLASS  A      ICE9B     ")
	    (GLASS         8   "GLASS  A      GLASS  B  ")
	    (FATSYNTH      9   "NOJOKE        FATSYNTHA2")
	    (SLO-GLASS1   10   "SlowGlass     SlowIce   ")
	    (PLK          11   "Pluck02       StrumNse2 ")
	    (BELL-PAD     12   "BellPad       SilkPad   ")
	    (SCHIMMER     13   "Schimmer01    Schimmer02")
	    (BRIGHT-ORGAN 14   "MayDayOrgn    maySoap   ")
	    (SLO-GLASS2   15   "SLW GLS A     LW GLS B  ")
	    (INIVOICE     32   "INIVOICE      INIVOICE  ")))

(defun tx-program-hook (tx-id program presets)
  (let* ((spec (assoc program presets))
	 (program-number 
	  (1- (cond (spec (second spec))
		    ((and (integerp program)
			  (plusp program)
			  (< program 33))
		     program)
		    (t (let ((frmt "~A is not valid TX~A program"))
			 (cyco-warning (format nil frmt program tx-id))
			 1))))))
    #'(lambda (time cindex prog _)
	(dismiss _)
	(if (eq prog :?)
	    (progn 
	      (format t "TX~A Program [~3d] ~A~%" tx-id (1+ program-number) program)
	      nil)
	  (list (cons time (midi-program-change cindex program-number)))))))
    
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
(let ((rem-a (format nil "TXA Programs:~%"))
      (rem-b (format nil "TXB Programs:~%"))
      (rem-c (format nil "TXC Programs:~%"))
      (frmt "[~3d] ~12A (~A)~%"))
  (dolist (p +TXA-PROGRAMS+)
    (setf rem-a (str+ rem-a (format nil frmt (second p)(first p)(third p)))))
  (dolist (p +TXB-PROGRAMS+)
    (setf rem-b (str+ rem-b (format nil frmt (second p)(first p)(third p)))))
  (dolist (p +TXC-PROGRAMS+)
    (setf rem-c (str+ rem-c (format nil frmt (second p)(first p)(third p)))))
  
  (param TXA (create-instrument 'TXA
				:parent TX816
				:transient nil
				:channel :TXA
				:remarks rem-a))

  (param TXB (create-instrument 'TXB
				:parent TX816
				:transient nil
				:channel :TXB
				:remarks rem-b))

  (param TXC (create-instrument 'TXC
				:parent TX816
				:transient nil
				:channel :TXC
				:remarks rem-c)))

(defmacro txa (name program &key
		       (keynumber-map nil)
		       (duration-map nil)
		       (amplitude-map nil))
  `(param ,name (create-instrument
		 ',name
		 :parent txa
		 :remarks ""
		 :transient t
		 :keynumber-map ,keynumber-map
		 :duration-map ,duration-map
		 :amplitude-map ,amplitude-map
		 :program-number ,program
		 :program-bank 0
		 :program-change-hook
		 (tx-program-hook 'A ,program +TXA-PROGRAMS+))))
		 
(defmacro txb (name program &key
		    (keynumber-map nil)
		    (duration-map nil)
		    (amplitude-map nil))
  `(param ,name (create-instrument
		 ',name
		 :parent txb
		 :remarks ""
		 :transient t
		 :keynumber-map ,keynumber-map
		 :duration-map ,duration-map
		 :amplitude-map ,amplitude-map
		 :program-number ,program
		 :program-bank 0
		 :program-change-hook 
		 (tx-program-hook 'B ,program +TXB-PROGRAMS+))))


(defmacro txc (name program &key
		       (keynumber-map nil)
		       (duration-map nil)
		       (amplitude-map nil))
  `(param ,name (create-instrument
		 ',name
		 :parent txc
		 :remarks ""
		 :transient t
		 :keynumber-map ,keynumber-map
		 :duration-map ,duration-map
		 :amplitude-map ,amplitude-map
		 :program-number ,program
		 :program-bank 0
		 :program-change-hook 
		 (tx-program-hook 'C ,program +TXC-PROGRAMS+))))
