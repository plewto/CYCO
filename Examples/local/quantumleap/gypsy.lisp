;;;; cyco local quantumleap gypsy
;;;;

(in-package :cyco)

(param gypsy1 (create-instrument 'gypsy1
				 :parent ql3
				 :transient nil))
				 
	       


;;; ---------------------------------------------------------------------- 
;;;				 Cimbalon

(param cimbalon nil)

(defun cimbalon (&key (parent gypsy1))
  (free-orchestra! :node parent)
  (setf cimbalon
	(create-instrument
	 'cimbalon
	 :parent parent
	 :transient t
	 :program-change-hook
	 (keyswitch 'cimbalon
		    '((sus      . (c2  "Sustain"  :range (c1 e5)))
		      (double   . (cs2 "Double"   :range (c1 e5)))
		      (tremoolo . (d2  "Tremolo"  :range (c1 e5))))))))

;;; ---------------------------------------------------------------------- 
;;;				 Castanets

(param gypsy-castanets nil)
(param gypsy-dancer nil)

(defun gypsy-castanets (&key (parent gypsy1))
  (free-orchestra! :node parent)
  (setf gypsy-castanets (create-instrument
			 'castanets
			 :parent parent
			 :transient t)))

(defun gypsy-dancer (&key (parent gypsy1))
  (free-orchestra! :node parent)
  (setf gypsy-dancer (create-instrument
		      'dancer
		      :parent parent
		      :transient t)))

;;; ---------------------------------------------------------------------- 
;;;				Percussion

(param gypsy-percussion nil)

(defun gypsy-percussion (&key (parent gypsy1))
  (free-orchestra! :node parent)
  (setf gypsy-castanets (create-instrument
			 'percussion
			 :parent parent
			 :transient t)))

;;; ---------------------------------------------------------------------- 
;;;				 Trombone

(param trombone nil)

(defun trombone (&key (parent gypsy1))
  (free-orchestra! :node parent)
  (setf trombone
	(create-instrument
	 'trombone
	 :parent parent
	 :transient t
	 :program-change-hook
	 (keyswitch 
	  'trombone
	  '((sus           . (C2  "Sustain"        :range (e1  d4)))
	    (sus-farty     . (cs2 "Sustain-Farty"  :range (e1  d4)))
	    (staccato      . (d2  "Staccato"       :range (e1  d4)))
	    (staccato-long . (ds2 "Staccato-long"  :range (e1  d4)))
	    (marc          . (e2  "Marc"           :range (e1  d4)))
	    (dimuendo      . (f2  "Dimuendo"       :range (e1  d4)))
	    (sforzando     . (fs2 "Sforzando"      :range (e1  d4)))
	    (crescendo     . (g2  "Crescendo"      :range (e1  d4))))))))

;;; ---------------------------------------------------------------------- 
;;;				  Violin

(param violin nil)

(defun violin (&key (parent gypsy1))
  (free-orchestra! :node parent)
  (setf violin 
	(create-instrument
	 'violin
	 :parent parent
	 :transient t
	 :program-change-hook
	 (keyswitch 'violin
		    '((sus-vib         . (C2  "sus-vib"         :range (g2  c6)))
		      (sus-vib2        . (CS2 "sus-vib2"        :range (g2  c6)))
		      (sus-vib3        . (D2  "sus-vib3"        :range (g2  c6)))
		      (passionato      . (DS2 "passionato"      :range (g2  c6)))
		      (expressive      . (E2  "expressive"      :range (g2  c6)))
		      (expressive-slow . (F2  "expressive-slow" :range (g2  c6)))
		      (sus-vib-accent  . (FS2 "sus-vib-accent"  :range (g2  c6)))
		      (sforzando       . (g2  "sforzando"       :range (g2  c6)))
		      (sul-tasto       . (gs2 "sul-tasto"       :range (g2  c6)))
		      (sul-tasto-exp   . (a2  "sul-tasto-exp"   :range (g2  c6)))
		      (accent-novib    . (as2 "accent-novib"    :range (g2  c6)))
		      (sforzando-novib . (b2  "aforzando-novib" :range (g2  c6)))
		      (sus-novib       . (c3  "sus-novib"       :range (g2  c6)))
		      (martele1        . (cs3 "martele1"        :range (g2  c6)))
		      (martele2        . (d3  "martele2"        :range (g2  c6)))
		      (spiccato        . (ds3 "spiccato"        :range (g2  c6)))
		      (spiccato2       . (e3  "spiccato2"       :range (g2  c6)))
		      (spiccato-long   . (f3  "spiccato-long"   :range (g2  c6)))
		      (spiccato-long2  . (fs3 "spiccato-long2"  :range (g2  c6)))
		      (left-pizzacato  . (g3  "left-pizzacato"  :range (g2  c6)))
		      (repitions       . (gs3 "repititions"     :range (g2  c6)))
		      (harmonics       . (a3  "harmonics"       :range (g3  a5)))
		      (ponticello      . (as3 "ponticello"      :range (g2  c6)))
		      (bounce          . (b3  "bounce"          :range (g2  c6))))))))
