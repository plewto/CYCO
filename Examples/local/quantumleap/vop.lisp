;;;; quatumleap vop
;;;;
;;;;     vop
;;;;     vop2
;;;;
;;;;        vop-american
;;;;        vop-american-oo
;;;;        vop-bulgarian
;;;;        vop-bulgarian-breath
;;;;        vop-bulgarian-deeper
;;;;        vop-indian
;;;;        vop-syrian
;;;;        vop-welsh
;;;;        vop-welsh-ah
;;;;        vop-welse-oh
;;;;        vop-welsh-words
;;;;

(in-package :cyco)

(let ((american-keyswitch 
       (keyswitch 'american
		  '((ah          C1      "Ah"             :range (c0 c9))
		    (ahoh-vib    CS1     "AhOh Vibrato"   :range (c0 c9))
		    (ah-vib      D1      "Ah Vibrato"     :range (c0 c9))
		    (ah-short    DS1     "Ah Short"       :range (c0 c9))
		    (ah-spooky   E1      "Ah Spooky"      :range (c0 c9))
		    (ah-staccato F1      "Ah Staccato"    :range (c0 c9)) 
		    (ah-vib      FS1     "Ah Vibrato"     :range (c0 c9))
		    (doo         G1      "Doo"            :range (c0 c9))
		    (dream       GS1     "Dream"          :range (c0 c9))
		    (ea-vib      A1      "Ea Vibrato"     :range (c0 c9))
		    (eaah        AS1     "EaAh"           :range (c0 c9))
		    (huoh        B1      "HuOh"           :range (c0 c9))
		    (mah         C2      "Mah"            :range (c0 c9))
		    (mei         CS2     "Mei"            :range (c0 c9))
		    (maom        D2      "Maom"           :range (c0 c9))
		    (mm          DS2     "Mm"             :range (c0 c9))
		    (muah        E2      "Muah"           :range (c0 c9))
		    (oh          F2      "Oh"             :range (c0 c9))
		    (oh-opera    FS2     "OhOpera"        :range (c0 c9))
		    (oh-vib      G2      "Oh Vibrato"     :range (c0 c9))
		    (ohm         GS2     "Ohm"            :range (c0 c9))
		    (ohm-vib     A2      "Ohm Vibrato"    :range (c0 c9))
		    (oo-breathy  AS2     "Oo Breathy"     :range (c0 c9))
		    (oo          B2      "Oo"             :range (c0 c9))
		    (oo-vib      C3      "Oo Vibrato"     :range (c0 c9))
		    (ah-rnd      CS3     "Ah Rnd"         :range (c0 c9))
		    (sigh        D3      "Sigh"           :range (c0 c9)))))
      (bulgarian-keyswitch 
       (keyswitch 'bulgarian
		  '((bul1            C1    "Bul1"        :range (c0 c9)) 
		    (bul2            CS1   "Bul2"        :range (c0 c9)) 
		    (bul3            D1    "Bul3"        :range (c0 c9)) 
		    (bul4            DS1   "Bul4"        :range (c0 c9)) 
		    (hiaheya         E1    "Hiaheya"     :range (c0 c9)) 
		    (melody1         F1    "melody1"     :range (c0 c9)) 
		    (melody2         FS1   "Melody2"     :range (c0 c9)) 
		    (oho-call-1      G1    "Oho-call-1"  :range (c0 c9)) 
		    (oho-call-2      GS1   "Oho-call-2"  :range (c0 c9)) 
		    (sheep-call      A1    "Sheep-call"  :range (c0 c9)) 
		    (oo              AS1   "Oo"          :range (c0 c9)) 
		    (oho             B1    "Oho"         :range (c0 c9)) 
		    (so              C2    "So"          :range (c0 c9)) 
		    (woho            CS2   "Woho"        :range (c0 c9)) 
		    (ihehoh          D2    "ihehoh"      :range (c0 c9)) 
		    (oho2            DS2   "Oho2"        :range (c0 c9)) 
		    (aha             E2    "Aha"         :range (c0 c9)) 
		    (heya            F2    "Heya"        :range (c0 c9)) 
		    (laha            FS2   "Laha"        :range (c0 c9)))))
      (indian-keyswitch 
       (keyswitch 'indian
		  '((i1  CS1 "Indian 1" :range (c0 c9))
		    (i2  DS1 "Indian 2" :range (c0 c9))
		    (i3  E1  "Indian 2" :range (c0 c9))
		    (i4  FS1 "Indian 2" :range (c0 c9))
		    (i5  GS1 "Indian 2" :range (c0 c9))
		    (i6  AS1 "Indian 2" :range (c0 c9))
		    (i7  B1  "Indian 2" :range (c0 c9))
		    (i8  GS2 "Indian 2" :range (c0 c9))
		    (i9  AS2 "Indian 2" :range (c0 c9))
		    (i10 B2  "Indian 2" :range (c0 c9)))))
      (syrian-keyswitch 
       (keyswitch 'syrian
		  '((c  C1  "key of C"   :range (c0 c9))
		    (cs CS1 "Key of cs"  :range (c0 c9))
		    (d  D1  "Key of d"   :range (c0 c9))
		    (ds DS1 "Key of ds"  :range (c0 c9))
		    (e  E1  "Key of e"   :range (c0 c9))
		    (f  F1  "Key of f"   :range (c0 c9))
		    (fs FS1 "Key of fs"  :range (c0 c9))
		    (g  G1  "Key of g"   :range (c0 c9))
		    (gs GS1 "Key of gs"  :range (c0 c9))
		    (a  A1  "Key of a"   :range (c0 c9))
		    (as AS1 "Key of as"  :range (c0 c9))
		    (b  B1  "Key of b"   :range (c0 c9)))))
      (welsh-keyswitch 
       (keyswitch 'welsh
		  '((c  C1  "key of C"   :range (c0 c9))
		    (cs CS1 "Key of cs"  :range (c0 c9))
		    (d  D1  "Key of d"   :range (c0 c9))
		    (ds DS1 "Key of ds"  :range (c0 c9))
		    (e  E1  "Key of e"   :range (c0 c9))
		    (f  F1  "Key of f"   :range (c0 c9))
		    (fs FS1 "Key of fs"  :range (c0 c9))
		    (g  G1  "Key of g"   :range (c0 c9))
		    (gs GS1 "Key of gs"  :range (c0 c9))
		    (a  A1  "Key of a"   :range (c0 c9))
		    (as AS1 "Key of as"  :range (c0 c9))
		    (b  B1  "Key of b"   :range (c0 c9)))))
      (welsh-words-keyswitch 
       (keyswitch 'welsh-words
		  '((bene    C1      "Bene"       :range (c0 c9))
		    (breath  CS1     "Breath"     :range (c0 c9))
		    (close   D1      "Close"      :range (c0 c9))
		    (dark    DS1     "Dark"       :range (c0 c9))
		    (death   E1      "Death"      :range (c0 c9))
		    (domini  F1      "Domini"     :range (c0 c9))
		    (dream   FS1     "Dream"      :range (c0 c9))
		    (drown   G1      "Drown"      :range (c0 c9))
		    (im      GS1     "Im"         :range (c0 c9))
		    (fall    A1      "Fall"       :range (c0 c9))
		    (fire    AS1     "Fire"       :range (c0 c9))
		    (fly     B1      "Fly"        :range (c0 c9))
		    (gaia    C2      "Gaia"       :range (c0 c9))
		    (grass   CS2     "Grass"      :range (c0 c9))
		    (hasan   D2      "Hasan"      :range (c0 c9))
		    (hate    DS2     "Hate"       :range (c0 c9))
		    (how     E2      "How"        :range (c0 c9))
		    (in      F2      "In"         :range (c0 c9))
		    (len     FS2     "Len"        :range (c0 c9))
		    (love    G2      "Love"       :range (c0 c9))
		    (luxet   GS2     "Luxet"      :range (c0 c9))
		    (ly      A2      "Ly"         :range (c0 c9))
		    (mei     AS2     "Mei"        :range (c0 c9))
		    (ness    B2      "Ness"       :range (c0 c9))
		    (of      C3      "Of"         :range (c0 c9))
		    (ooze    CS3     "Ooze"       :range (c0 c9))
		    (pray    D3      "Pray"       :range (c0 c9))
		    (preist  DS3     "Preist"     :range (c0 c9))
		    (row     E3      "Row"        :range (c0 c9))
		    (ruins   F3      "Ruins"      :range (c0 c9))
		    (run     FS3     "Run"        :range (c0 c9))
		    (san     G3      "San"        :range (c0 c9))
		    (sing    GS3     "Sing"       :range (c0 c9))
		    (so      A3      "So"         :range (c0 c9))
		    (soft    AS3     "Soft"       :range (c0 c9))
		    (this    B3      "This"       :range (c0 c9))
		    (true    C4      "True"       :range (c0 c9))
		    (uram    CS4     "Uram"       :range (c0 c9))
		    (ventius D4      "Ventius"    :range (c0 c9))
		    (ver     DS4     "Ver"        :range (c0 c9))
		    (vosh    E4      "Vosh"       :range (c0 c9))
		    (fortuna F4      "Fortuna"    :range (c0 c9))
		    (from    FS4     "From"       :range (c0 c9))
		    (gravis  G4      "Gravis"     :range (c0 c9))
		    (is      GS4     "Is"         :range (c0 c9))
		    (rain    A4      "Rain"       :range (c0 c9))
		    (the     AS4     "The"        :range (c0 c9)))))
      (remtext (str+ (format nil "Voices Of Passion:~%")
		     (format nil "    VOP-American~%")
		     (format nil "    VOP-American-oo~%")
		     (format nil "    VOP-Bulgarian~%")
		     (format nil "    VOP-Bulgarian-breath~%")
		     (format nil "    VOP-Bulgarian-deeper~%")
		     (format nil "    VOP-Indian~%")
		     (format nil "    VOP-Syrian  (In specific keys)~%")
		     (format nil "    VOP-Welsh~%")
		     (format nil "    VOP-Welsh-ah~%")
		     (format nil "    VOP-Welsh-oh~%")
		     (format nil "    VOP-Welsh-words~%"))))
  (param vop (create-instrument 'vop
				:parent ql4
				:transient nil
				:remarks remtext))
  (param vop2 (create-instrument 'vop2
				 :parent quantumleap
				 :channel :vop2
				 :transient nil
				 :remarks remtext))

  (defun vop-american (&key (parent vop))
    (param vop-american (create-instrument 'vop-american
					   :parent parent
					   :remarks ""
					   :program-change-hook american-keyswitch))
    vop-american)

  (defun vop-american-oo (&key (parent vop))
    (param vop-american-oo (create-instrument 'vop-american-oo
					      :remarks ""
					      :parent parent))
    vop-american-oo)

  (defun vop-bulgarian (&key (parent vop))
    (param vop-bulgarian (create-instrument
			  'vop-bulgarian
			  :parent parent
			  :remarks ""
			  :program-change-hook bulgarian-keyswitch))
    vop-bulgarian)
  
  (defun vop-bulgarian-breath (&key (parent vop))
    (param vop-bulgarian-breath (create-instrument
				 'vop-bulgarian-breath
				 :remarks ""
				 :parent parent))
    vop-bulgarian-breath)

  (defun vop-bulgarian-deeper (&key (parent vop))
    (param vop-bulgarian-deeper
	   (create-instrument 'vop-bulgarian-deeper
			      :remarks ""
			      :parent parent))
    vop-bulgarian-deeper)

  (defun vop-indian (&key (parent vop))
    (param vop-indian (create-instrument 'vop-indian
					 :remarks ""
					 :parent parent
					 :program-change-hook indian-keyswitch))
    vop-indian)
  
  (defun vop-syrian (&key (parent vop))
    (param vop-syrian (create-instrument 'vop-syrian
					 :remarks ""
					 :parent parent
					 :program-change-hook syrian-keyswitch))
    vop-syrian)
  
  (defun vop-welsh (&key (parent vop))
    (param vop-welsh (create-instrument 'vop-welsh
					:remarks ""
					:parent parent
					:program-change-hook welsh-keyswitch))
    vop-welsh)
  
  (defun vop-welsh-ah (&key (parent vop))
    (param vop-welsh-ah (create-instrument 'vop-welsh-ah
					   :remarks ""
					   :parent parent))
    vop-welsh-ah)

  (defun vop-welsh-oh (&key (parent vop))
    (param vop-welsh-oh (create-instrument 'vop-welsh-oh
					   :remarks ""
					   :parent parent))
    vop-welsh-oh)

  (defun vop-welsh-words (&key (parent vop))
    (param vop-welsh-words (create-instrument
			    'vop-welsh-words
			    :remarks ""
			    :parent parent
			    :program-change-hook welsh-words-keyswitch))
    vop-welsh-words)) 
  
  

  
  
  
  
		       
  
  
  
  
		       
		       
			       
  
