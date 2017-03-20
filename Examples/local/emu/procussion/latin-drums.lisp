;;;; cyco/Local/Emu/Procusion/latin-drums.lisp
;;;; 2012.09.02
;;;; Instruments
;;;;    latin-drums
;;;;        timbale
;;;;        timbale2
;;;;        tumba
;;;;        quinto
;;;;        hembra
;;;;        macho
;;;;        ld-conga
;;;;        ld-shaker
;;;;

(in-package :cyco)

(param latin-drums nil)

(let ((timbale1-map (keymap 'timbale1
			    '((A      39)
			      (B      42)
			      (stick  36)
			      (A2     40)
			      (B2     43)
			      (stick2 37)
			      (A3     41)
			      (B3     44)
			      (stick3 38))))
      (timbale2-map (keymap 'timbale2
			    '((A      48)
			      (B      51)
			      (stcik  45)
			      (A2     49)
			      (B2     52)
			      (stick2 46)
			      (A3     50)
			      (B3     53)
			      (stick3 47))))
      (tumba-map (keymap 'tumba '((A     54 tone)
				  (RIM   57)
				  (A2    55 tone)
				  (RIM2  58)
				  (A3    56 tone)
				  (RIM3  59))))
      (quinto-map (keymap 'quinto '((A      60 tone)
				    (SLAP   63 open-slap)
				    (CSLAP  66 closed-slap)
				    (TIP    69)
				    (HEEL   72)
				    (A2     61 tone)
				    (TIP2   70)
				    (SLAP2  65 open-slap)
				    (CSLAP2 67 closed-slap)
				    (HEEL2  73)
				    (A3     62 tone)
				    (SLAP3  64 open-slap)
				    (CSLAP3 68 closed-slap)
				    (TIP3   71)
				    (HEEL3  74))))
      (hembra-map (keymap 'hembra '((A       75 tone)
				    (SLAP   57)
				    (A2     76 tone)
				    (SLAP2  58)
				    (A3     77 tone)
				    (SLAP3  59))))
      (macho-map (keymap 'macho '((A     90 tone)
				  (SLAP  93)
				  (RIM   87)
				  (A2    91 tone)
				  (SLAP2 94)
				  (RIM2  88)
				  (A3    92 tone)
				  (SLAP3 95)
				  (RIM3  89)
				  (TIP   81 LEFT)
				  (TIP2  84 RIGHT)
				  (TIP3  82 LEFT)
				  (TIP4  85 RIGHT)
				  (TIP5  83 LEFT)
				  (TIP6  86 RIGHT))))
      (conga-map (keymap 'slide-conga '((A    098)
					(B    100)
					(B2   101)
					(B3   102))))
      (shaker-map (keymap 'shaker '((A 078)
				    (B 066))))

      (conga-set-map (keymap 'conga-set ; tumba and quinto combined
			     '((TUMBA  54  tumba tone)
			       (RIM    57  tumba rim)
			       (QUINTO 60  quinto tone)
			       (SLAP   63  quinto slap)
			       (CSLAP  67  quinto closed-slap)
			       (TIP    69  quinto tip)
			       (HEEL   72  QUINTO HEEL))))
      (program-number (car (cdr (assoc 'latin-drums +PROCUSSION-PROGRAMS+)))) ) 
  (setf latin-drums nil)

  (defun latin-drums (&key (parent pro3))
    (setf latin-drums (create-instrument
    		       'latin-drums
    		       :parent parent
    		       :transient t
    		       :program-change-hook (constant-program-hook
    					     'latin-drums program-number)))
    (param timbale1 (create-instrument
    		    'timbale1
    		    :parent latin-drums
    		    :keynumber-map timbale1-map))
    (param timbale2 (create-instrument
    		     'timbale2
    		     :parent latin-drums
    		     :keynumber-map timbale2-map))
    (param tumba (create-instrument
    		  'tumba
    		  :parent latin-drums
    		  :keynumber-map tumba-map))
    (param quinto (create-instrument
    		   'quinto
    		   :parent latin-drums
    		   :keynumber-map quinto-map))
    (param hembra (create-instrument
    		   'hembra
    		   :parent latin-drums
    		   :keynumber-map hembra-map))
    (param macho (create-instrument
    		  'macho
    		  :parent latin-drums
    		  :keynumber-map macho-map))
    (param ld-conga (create-instrument
    		     'ld-conga
    		     :parent latin-drums
    		     :keynumber-map conga-map))
    (param ld-shaker (create-instrument
    		      'ld-shaker
    		      :parent latin-drums
    		      :keynumber-map shaker-map))
    (param conga-set (create-instrument
    		      'ld-conga-set
    		      :parent latin-drums
    		      :keynumber-map conga-set-map))
    latin-drums))
