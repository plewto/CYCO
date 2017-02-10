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

(defmap --latin-drums-timbale-1-map  '((A      . 39)
				       (B      . 42)
				       (stick  . 36)
				       (A2     . 40)
				       (B2     . 43)
				       (stick2 . 37)
				       (A3     . 41)
				       (B3     . 44)
				       (stick3 . 38)))

(defmap --latin-drums-timbale-2-map '((A       . 48)
				      (B       . 51)
				      (stcik   . 45)
				      (A2      . 49)
				      (B2      . 52)
				      (stick2  . 46)
				      (A3      . 50)
				      (b3      . 53)
				      (stick3  . 47)))

(defmap --latin-drums-tumba-map '((A     . (54 tone))
				  (RIM   .  57)
				  (A2    . (55 tone))
				  (RIM2  .  58)
				  (A3    . (56 tone))
				  (RIM3  .  59)))

(defmap --latin-drums-quinto-map '((A      . (60 tone))
				   (SLAP   . (63 open-slap))
				   (CSLAP  . (66 closed-slap))
				   (TIP    .  69)
				   (HEEL   .  72)
				   (A2     . (61 tone))
				   (TIP2   .  70)
				   (SLAP2  . (65 open-slap))
				   (CSLAP2 . (67 closed-slap))
				   (HEEL2  .  73)
				   (A3     . (62 tone))
				   (SLAP3  . (64 open-slap))
				   (CSLAP3 . (68 closed-slap))
				   (TIP3   .  71)
				   (HEEL3  .  74)))

(defmap --latin-drums-hembra-map '((A      . (75 tone))
				   (SLAP   .  57)
				   (A2     . (76 tone))
				   (SLAP2  .  58)
				   (A3     . (77 tone))
				   (SLAP3  .  59)))

(defmap --latin-drums-macho-map '((A      . (90 tone))
				  (SLAP   .  93)
				  (RIM    .  87)
				  (A2     . (91 tone))
				  (SLAP2  .  94)
				  (RIM2   .  88)
				  (A3     . (92 tone))
				  (SLAP3  .  95)
				  (RIM3   .  89)
				  (TIP    . (81 LEFT))
				  (TIP2   . (84 RIGHT))
				  (TIP3   . (82 LEFT))
				  (TIP4   . (85 RIGHT))
				  (TIP5   . (83 LEFT))
				  (TIP6   . (86 RIGHT))))


(defmap --latin-drums-slide-conga-map '((A  . 098)
					(B  . 100)
					(B2 . 101)
					(B3 . 102)))

(defmap --latin-drums-shaker-map '((A . 078)
				   (B . 066)))

(defmap --latin-drums-conga-set-map		; tumba and quinto combined
  '((TUMBA     .   (54  tumba tone))
    (RIM       .   (57  tumba rim))
    (QUINTO    .   (60  quinto tone))
    (SLAP      .   (63  quinto slap))
    (CSLAP     .   (67  quinto closed-slap))
    (TIP       .   (69  quinto tip))
    (HEEL      .   (72  QUINTO HEEL))))
	       



(param latin-drums nil)

(defun latin-drums (&optional (parent pro4))
  (remove-children parent)
  (set-value parent :program (procussion-program-map 'latin-drums))
  (let ((kinst (instrument 'latin-drums
			   :parent parent)))
    (param timbale1 (instrument :timbale1
				:parent kinst
				:keymap #'--latin-drums-timbale-1-map))
    (param timbale2 (instrument :timbale2
				:parent kinst
				:keymap #'--latin-drums-timbale-2-map))
    (param tumba (instrument :tumba
			     :parent kinst
			     :keymap #'--latin-drums-tumba-map))
    (param quinto (instrument :quinto
			      :parent kinst
			      :keymap #'--latin-drums-quinto-map))
    (param hembra (instrument :hembra
			    :parent kinst
			    :keymap #'--latin-drums-hembra-map))
    (param macho (instrument :macho
			     :parent kinst
			     :keymap #'--latin-drums-macho-map))
    (param ld-conga (instrument :ld-conga
				:parent kinst
				:keymap #'--latin-drums-slide-conga-map))
    (param ld-shaker (instrument :ld-shaker
				 :parent kinst
				 :keymap #'--latin-drums-shaker-map))
    (param ld-conga-set (instrument :ld-conga-set
				    :parent kinst
				    :keymap #'--latin-drums-conga-set-map))
    (setf latin-drums kinst)
    kinst))
