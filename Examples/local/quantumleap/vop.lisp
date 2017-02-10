;;;; cyco local quantumleap vop
;;;;

(in-package :cyco)

(param vop1 (create-instrument
	    'VOP1
	    :parent quantumleap
	    :transient nil
	    :channel :VOP1))

(param vop2 (create-instrument
	     'VOP2
	     :parent quantumleap
	     :transient nil
	     :channel :VOP2))

;;; ---------------------------------------------------------------------- 
;;;				 American

(param vop-american nil)
(param vop-american-oo nil)	   

(param --vop-american-keyswitch
       (keyswitch 'vop-american
		  '((ah          . (C2      "Ah"             :range (c0 c9)))
		    (ahoh-vib    . (CS2     "AhOh Vibrato"   :range (c0 c9)))
		    (ah-vib      . (D2      "Ah Vibrato"     :range (c0 c9)))
		    (ah-short    . (DS2     "Ah Short"       :range (c0 c9)))
		    (ah-spooky   . (E2      "Ah Spooky"      :range (c0 c9)))
		    (ah-staccato . (F2      "Ah Staccato"    :range (c0 c9)))
		    (ah-vib      . (Fs2     "Ah Vibrato"     :range (c0 c9)))
		    (doo         . (G2      "Doo"            :range (c0 c9)))
		    (dream       . (GS2     "Dream"          :range (c0 c9)))
		    (ea-vib      . (A2      "Ea Vibrato"     :range (c0 c9)))
		    (eaah        . (AS2     "EaAh"           :range (c0 c9)))
		    (huoh        . (B2      "HuOh"           :range (c0 c9)))
		    (mah         . (C3      "Mah"            :range (c0 c9)))
		    (mei         . (CS3     "Mei"            :range (c0 c9)))
		    (maom        . (D3      "Maom"           :range (c0 c9)))
		    (mm          . (DS3     "Mm"             :range (c0 c9)))
		    (muah        . (E3      "Muah"           :range (c0 c9)))
		    (oh          . (F3      "Oh"             :range (c0 c9)))
		    (oh-opera    . (FS3     "OhOpera"        :range (c0 c9)))
		    (oh-vib      . (G3      "Oh Vibrato"     :range (c0 c9)))
		    (ohm         . (GS3     "Ohm"            :range (c0 c9)))
		    (ohm-vib     . (A3      "Ohm Vibrato"    :range (c0 c9)))
		    (oo-breathy  . (AS3     "Oo Breathy"     :range (c0 c9)))
		    (oo          . (B3      "Oo"             :range (c0 c9)))
		    (oo-vib      . (C4      "Oo Vibrato"     :range (c0 c9)))
		    (ah-rnd      . (CS4     "Ah Rnd"         :range (c0 c9)))
		    (sigh        . (D4      "Sigh"           :range (c0 c9))))))


(defun vop-american (&key (parent vop1))
  (free-orchestra! :node parent)
  (setf vop-american (create-instrument
		      'vop-american
		      :parent parent
		      :transient t
		      :program-change-hook --vop-american-keyswitch)))

(defun vop-american-oo (&key (parent vop1))
  (free-orchestra! :node parent)
  (setf vop-american-oo (create-instrument
			 'vop-american-oo
			 :parent parent
			 :transient t)))

;;; ---------------------------------------------------------------------- 
;;;				 Bulgarian

(param vop-bulgarian nil)
(param vop-bulgarian-breath nil)
(param vop-bulgarian-breath-deeper nil)

(param  --vop-bulgarian-keyswitch
  (keyswitch 'vop-bulgarian
  '((bul1            . (C2    "Bul1"        :range (c0 c9))) 
    (bul2            . (CS2   "Bul2"        :range (c0 c9))) 
    (bul3            . (D2    "Bul3"        :range (c0 c9))) 
    (bul4            . (DS2   "Bul4"        :range (c0 c9))) 
    (hiaheya         . (E2    "Hiaheya"     :range (c0 c9))) 
    (melody1         . (F2    "melody1"     :range (c0 c9))) 
    (melody2         . (FS2   "Melody2"     :range (c0 c9))) 
    (oho-call-1      . (G2    "Oho-call-1"  :range (c0 c9))) 
    (oho-call-2      . (GS2   "Oho-call-2"  :range (c0 c9))) 
    (sheep-call      . (A2    "Sheep-call"  :range (c0 c9))) 
    (oo              . (AS2   "Oo"          :range (c0 c9))) 
    (oho             . (B2    "Oho"         :range (c0 c9))) 
    (so              . (C3    "So"          :range (c0 c9))) 
    (woho            . (CS3   "Woho"        :range (c0 c9))) 
    (ihehoh          . (D3    "ihehoh"      :range (c0 c9))) 
    (oho2            . (DS3   "Oho2"        :range (c0 c9))) 
    (aha             . (E3    "Aha"         :range (c0 c9))) 
    (heya            . (F3    "Heya"        :range (c0 c9))) 
    (laha            . (FS3   "Laha"        :range (c0 c9))))))

(defun vop-bulgarian (&optional (parent vop1))
  (free-orchestra! :node parent)
  (setf vop-bulgarian (create-instrument
		       'vop-bulgarian
		       :parent parent
		       :transient t
		       :program-change-hook --vop-bulgarian-keyswitch)))

(defun vop-bulgarian-breath (&optional (parent vop1))
  (free-orchestra! :node parent)
  (setf vop-bulgarian-breath (create-instrument
			      'vop-bulgarian-breath
			      :parent parent
			      :transient t)))

(defun vop-bulgarian-breath-deeper (&optional (parent vop1))
  (free-orchestra! :node parent)
  (setf vop-bulgarian-breath-deeper (create-instrument
				     'vop-bulgarian-breath
				     :parent parent
				     :transient t))) 
;;; ---------------------------------------------------------------------- 
;;;				  Indian

(param vop-indian nil)

(param --vop-indian-keyswitch
       (keyswitch vop-indian
		  '((i1  . (CS2 "Indian 1" :range (c0 c9)))
		    (i2  . (DS2 "Indian 2" :range (c0 c9)))
		    (i3  . (E2  "Indian 2" :range (c0 c9)))
		    (i4  . (FS2 "Indian 2" :range (c0 c9)))
		    (i5  . (GS2 "Indian 2" :range (c0 c9)))
		    (i6  . (AS2 "Indian 2" :range (c0 c9)))
		    (i7  . (B2  "Indian 2" :range (c0 c9)))
		    (i8  . (GS3 "Indian 2" :range (c0 c9)))
		    (i9  . (AS3 "Indian 2" :range (c0 c9)))
		    (i10 . (B3  "Indian 2" :range (c0 c9))))))
       
(defun vop-indian (&key (parent vop1))
  (free-orchestra! :node parent)
  (setf vop-indian (create-instrument
		    'vop-indian
		    :parent parent
		    :transient t
		    :program-change-hook --vop-indian-keyswitch)))

;;; ---------------------------------------------------------------------- 
;;;				  syrian

(param vop-syrian nil)

(param --vop-syrian-keyswitch
       (keyswitch 'vop-syrian
		  '((c  . (c2  "key of C"   :range (c0 c9)))
		    (cs . (cs2 "Key of cs"  :range (c0 c9)))
		    (d  . (d2  "Key of d"   :range (c0 c9)))
		    (ds . (ds2 "Key of ds"  :range (c0 c9)))
		    (e  . (e2  "Key of e"   :range (c0 c9)))
		    (f  . (f2  "Key of f"   :range (c0 c9)))
		    (fs . (fs2 "Key of fs"  :range (c0 c9)))
		    (g  . (g2  "Key of g"   :range (c0 c9)))
		    (gs . (gs2 "Key of gs"  :range (c0 c9)))
		    (a  . (a2  "Key of a"   :range (c0 c9)))
		    (as . (as2 "Key of as"  :range (c0 c9)))
		    (b  . (b2  "Key of b"   :range (c0 c9))))))

(defun vop-syrian (&key (parent vop1))
  (free-orchestra! :node parent)
  (setf vop-syrian (create-instrument
		    'vop-syrian
		    :parent parent
		    :transient t
		    :program-change-hook --vop-syrian-keyswitch)))

;;; ---------------------------------------------------------------------- 
;;;				   Wales

(param vop-wales nil)
(param vop-wales-words nil)
(param vop-wales-ah nil)
(param vop-wales-oh nil)

(param --vop-wales-vowels-keyswitch
       (keyswitch 'vop-wales-vowels
		  '((ah    . (C2  "Ah"   :range (c0 c9)))
		    (oh    . (CS2 "Oh"   :range (c0 c9)))
		    (ee    . (D2  "Ee"   :range (c0 c9)))
		    (oo    . (DS2 "Oo"   :range (c0 c9)))
		    (mm    . (E2  "Mm"   :range (c0 c9))))))

(param --vop-wales-words-keyswitch
    (keyswitch 'vop-wales-words
	       '((bene    . (C2      "Bene"       :range (c0 c9)))
		 (breath  . (CS2     "Breath"     :range (c0 c9)))
		 (close   . (D2      "Close"      :range (c0 c9)))
		 (dark    . (DS2     "Dark"       :range (c0 c9)))
		 (death   . (E2      "Death"      :range (c0 c9)))
		 (domini  . (F2      "Domini"     :range (c0 c9)))
		 (dream   . (FS2     "Dream"      :range (c0 c9)))
		 (drown   . (G2      "Drown"      :range (c0 c9)))
		 (im      . (GS2     "Im"         :range (c0 c9)))
		 (fall    . (A2      "Fall"       :range (c0 c9)))
		 (fire    . (AS2     "Fire"       :range (c0 c9)))
		 (fly     . (B2      "Fly"        :range (c0 c9)))

		 
		 (gaia    . (C3      "Gaia"       :range (c0 c9)))
		 (grass   . (CS3     "Grass"      :range (c0 c9)))
		 (hasan   . (D3      "Hasan"      :range (c0 c9)))
		 (hate    . (DS3     "Hate"       :range (c0 c9)))
		 (how     . (E3      "How"        :range (c0 c9)))
		 (in      . (F3      "In"         :range (c0 c9)))
		 (len     . (FS3     "Len"        :range (c0 c9)))
		 (love    . (G3      "Love"       :range (c0 c9)))
		 (luxet   . (GS3     "Luxet"      :range (c0 c9)))
		 (ly      . (A3      "Ly"         :range (c0 c9)))
		 (mei     . (AS3     "Mei"        :range (c0 c9)))
		 (ness    . (B3      "Ness"       :range (c0 c9)))
		 
		 (of      . (C4      "Of"         :range (c0 c9)))
		 (ooze    . (CS4     "Ooze"       :range (c0 c9)))
		 (pray    . (D4      "Pray"       :range (c0 c9)))
		 (preist  . (DS4     "Preist"     :range (c0 c9)))
		 (row     . (E4      "Row"        :range (c0 c9)))
		 (ruins   . (F4      "Ruins"      :range (c0 c9)))
		 (run     . (FS4     "Run"        :range (c0 c9)))
		 (san     . (G4      "San"        :range (c0 c9)))
		 (sing    . (GS4     "Sing"       :range (c0 c9)))
		 (so      . (A4      "So"         :range (c0 c9)))
		 (soft    . (AS4     "Soft"       :range (c0 c9)))
		 (this    . (B4      "This"       :range (c0 c9)))
		 
		 (true    . (C5      "True"       :range (c0 c9)))
		 (uram    . (CS5     "Uram"       :range (c0 c9)))
		 (ventius . (D5      "Ventius"    :range (c0 c9)))
		 (ver     . (DS5     "Ver"        :range (c0 c9)))
		 (vosh    . (E5      "Vosh"       :range (c0 c9)))
		 (fortuna . (F5      "Fortuna"    :range (c0 c9)))
		 (from    . (FS5     "From"       :range (c0 c9)))
		 (gravis  . (G5      "Gravis"     :range (c0 c9)))
		 (is      . (GS5     "Is"         :range (c0 c9)))
		 (rain    . (A5      "Rain"       :range (c0 c9)))
		 (the     . (AS5     "The"        :range (c0 c9))))))


(defun vop-wales-ah (&key (parent vop1))
  (free-orchestra! :node parent)
  (setf vop-wales-ah (create-instrument
		      'vop-wales-ah
		      :parent parent
		      :transient t)))

(defun vop-wales-oh (&key (parent vop1))
  (free-orchestra! :node parent)
  (setf vop-wales-oh (create-instrument
		      'vop-wales-oh
		      :parent parent
		      :transient t)))

(defun vop-wales (&key (parent vop1))
  (free-orchestra! :node parent)
  (setf vop-wales-oh (create-instrument
		      'vop-wales
		      :parent parent
		      :transient t
		      :program-change-hook --vop-wales-vowels-keyswitch)))

(defun vop-wales-words (&key (parent vop1))
  (free-orchestra! :node parent)
  (setf vop-wales-oh (create-instrument
		      'vop-wales-words
		      :parent parent
		      :transient t
		      :program-change-hook --vop-wales-words-keyswitch)))


