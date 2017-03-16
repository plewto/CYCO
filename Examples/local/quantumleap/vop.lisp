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
		  '((ah          C2      "Ah"             :range (c0 c9))
		    (ahoh-vib    CS2     "AhOh Vibrato"   :range (c0 c9))
		    (ah-vib      D2      "Ah Vibrato"     :range (c0 c9))
		    (ah-short    DS2     "Ah Short"       :range (c0 c9))
		    (ah-spooky   E2      "Ah Spooky"      :range (c0 c9))
		    (ah-staccato F2      "Ah Staccato"    :range (c0 c9)) 
		    (ah-vib      FS2     "Ah Vibrato"     :range (c0 c9))
		    (doo         G2      "Doo"            :range (c0 c9))
		    (dream       GS2     "Dream"          :range (c0 c9))
		    (ea-vib      A2      "Ea Vibrato"     :range (c0 c9))
		    (eaah        AS2     "EaAh"           :range (c0 c9))
		    (huoh        B2      "HuOh"           :range (c0 c9))
		    (mah         C3      "Mah"            :range (c0 c9))
		    (mei         CS3     "Mei"            :range (c0 c9))
		    (maom        D3      "Maom"           :range (c0 c9))
		    (mm          DS3     "Mm"             :range (c0 c9))
		    (muah        E3      "Muah"           :range (c0 c9))
		    (oh          F3      "Oh"             :range (c0 c9))
		    (oh-opera    FS3     "OhOpera"        :range (c0 c9))
		    (oh-vib      G3      "Oh Vibrato"     :range (c0 c9))
		    (ohm         GS3     "Ohm"            :range (c0 c9))
		    (ohm-vib     A3      "Ohm Vibrato"    :range (c0 c9))
		    (oo-breathy  AS3     "Oo Breathy"     :range (c0 c9))
		    (oo          B3      "Oo"             :range (c0 c9))
		    (oo-vib      C4      "Oo Vibrato"     :range (c0 c9))
		    (ah-rnd      CS4     "Ah Rnd"         :range (c0 c9))
		    (sigh        D4      "Sigh"           :range (c0 c9)))))
      (bulgarian-keyswitch 
       (keyswitch 'bulgarian
		  '((bul1            C2    "Bul1"        :range (c0 c9)) 
		    (bul2            CS2   "Bul2"        :range (c0 c9)) 
		    (bul3            D2    "Bul3"        :range (c0 c9)) 
		    (bul4            DS2   "Bul4"        :range (c0 c9)) 
		    (hiaheya         E2    "Hiaheya"     :range (c0 c9)) 
		    (melody1         F2    "melody1"     :range (c0 c9)) 
		    (melody2         FS2   "Melody2"     :range (c0 c9)) 
		    (oho-call-1      G2    "Oho-call-1"  :range (c0 c9)) 
		    (oho-call-2      GS2   "Oho-call-2"  :range (c0 c9)) 
		    (sheep-call      A2    "Sheep-call"  :range (c0 c9)) 
		    (oo              AS2   "Oo"          :range (c0 c9)) 
		    (oho             B2    "Oho"         :range (c0 c9)) 
		    (so              C3    "So"          :range (c0 c9)) 
		    (woho            CS3   "Woho"        :range (c0 c9)) 
		    (ihehoh          D3    "ihehoh"      :range (c0 c9)) 
		    (oho2            DS3   "Oho2"        :range (c0 c9)) 
		    (aha             E3    "Aha"         :range (c0 c9)) 
		    (heya            F3    "Heya"        :range (c0 c9)) 
		    (laha            FS3   "Laha"        :range (c0 c9)))))
      (indian-keyswitch 
       (keyswitch 'indian
		  '((i1  CS2 "Indian 1" :range (c0 c9))
		    (i2  DS2 "Indian 2" :range (c0 c9))
		    (i3  E2  "Indian 2" :range (c0 c9))
		    (i4  FS2 "Indian 2" :range (c0 c9))
		    (i5  GS2 "Indian 2" :range (c0 c9))
		    (i6  AS2 "Indian 2" :range (c0 c9))
		    (i7  B2  "Indian 2" :range (c0 c9))
		    (i8  GS3 "Indian 2" :range (c0 c9))
		    (i9  AS3 "Indian 2" :range (c0 c9))
		    (i10 B3  "Indian 2" :range (c0 c9)))))
      (syrian-keyswitch 
       (keyswitch 'syrian
		  '((c  C2  "key of C"   :range (c0 c9))
		    (cs CS2 "Key of cs"  :range (c0 c9))
		    (d  D2  "Key of d"   :range (c0 c9))
		    (ds DS2 "Key of ds"  :range (c0 c9))
		    (e  E2  "Key of e"   :range (c0 c9))
		    (f  F2  "Key of f"   :range (c0 c9))
		    (fs FS2 "Key of fs"  :range (c0 c9))
		    (g  G2  "Key of g"   :range (c0 c9))
		    (gs GS2 "Key of gs"  :range (c0 c9))
		    (a  A2  "Key of a"   :range (c0 c9))
		    (as AS2 "Key of as"  :range (c0 c9))
		    (b  B2  "Key of b"   :range (c0 c9)))))
      (welsh-keyswitch
       (keyswitch 'welsh
		  '((AH C2)
		    (OH CS2)
		    (EE D2)
		    (OO DS2)
		    (MM E2))))
      (welsh-words-keyswitch 
       (keyswitch 'welsh-words
		  '((bene    C2      "Bene"       :range (c0 c9))
		    (breath  CS2     "Breath"     :range (c0 c9))
		    (close   D2      "Close"      :range (c0 c9))
		    (dark    DS2     "Dark"       :range (c0 c9))
		    (death   E2      "Death"      :range (c0 c9))
		    (domini  F2      "Domini"     :range (c0 c9))
		    (dream   FS2     "Dream"      :range (c0 c9))
		    (drown   G2      "Drown"      :range (c0 c9))
		    (im      GS2     "Im"         :range (c0 c9))
		    (fall    A2      "Fall"       :range (c0 c9))
		    (fire    AS2     "Fire"       :range (c0 c9))
		    (fly     B2      "Fly"        :range (c0 c9))
		    (gaia    C3      "Gaia"       :range (c0 c9))
		    (grass   CS3     "Grass"      :range (c0 c9))
		    (hasan   D3      "Hasan"      :range (c0 c9))
		    (hate    DS3     "Hate"       :range (c0 c9))
		    (how     E3      "How"        :range (c0 c9))
		    (in      F3      "In"         :range (c0 c9))
		    (len     FS3     "Len"        :range (c0 c9))
		    (love    G3      "Love"       :range (c0 c9))
		    (luxet   GS3     "Luxet"      :range (c0 c9))
		    (ly      A3      "Ly"         :range (c0 c9))
		    (mei     AS3     "Mei"        :range (c0 c9))
		    (ness    B3      "Ness"       :range (c0 c9))
		    (of      C4      "Of"         :range (c0 c9))
		    (ooze    CS4     "Ooze"       :range (c0 c9))
		    (pray    D4      "Pray"       :range (c0 c9))
		    (preist  DS4     "Preist"     :range (c0 c9))
		    (row     E4      "Row"        :range (c0 c9))
		    (ruins   F4      "Ruins"      :range (c0 c9))
		    (run     FS4     "Run"        :range (c0 c9))
		    (san     G4      "San"        :range (c0 c9))
		    (sing    GS4     "Sing"       :range (c0 c9))
		    (so      A4      "So"         :range (c0 c9))
		    (soft    AS4     "Soft"       :range (c0 c9))
		    (this    B4      "This"       :range (c0 c9))
		    (true    C5      "True"       :range (c0 c9))
		    (uram    CS5     "Uram"       :range (c0 c9))
		    (ventius D5      "Ventius"    :range (c0 c9))
		    (ver     DS5     "Ver"        :range (c0 c9))
		    (vosh    E5      "Vosh"       :range (c0 c9))
		    (fortuna F5      "Fortuna"    :range (c0 c9))
		    (from    FS5     "From"       :range (c0 c9))
		    (gravis  G5      "Gravis"     :range (c0 c9))
		    (is      GS5     "Is"         :range (c0 c9))
		    (rain    A5      "Rain"       :range (c0 c9))
		    (the     AS5     "The"        :range (c0 c9)))))
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
  
  

  
  
  
  
		       
  
  
  
  
		       
		       
			       
  
