;;;; cyco local quantumleap gypsy-guitars
;;;;

(in-package :cyco)

;;; ---------------------------------------------------------------------- 
;;;				 Classical

(param classical-guitar nil)

(defun classical-guitar (&key (parent QL2))
  (free-orchestra! :node parent)
  (setf classical-guitar
	(create-instrument
	 'classical-guitar
	 :parent parent
	 :transient t
	 :program-change-hook
	 (keyswitch 'classical-guitar
		    '((sus        . (c2  "sus"        :range (e2 a5)))
		      (sus-vib    . (cs2 "sus-vib"    :range (e2 a5)))
		      (legato     . (d2  "legato"     :range (e2 a5)))
		      (harmonics  . (ds2 "harmonics"  :range (e2 e5)))
		      (mute-strum . (e2  "mute-strum" :range (e2 a5)))
		      (fret-noise . (f2  "fret-noise" :range (e2 c7))))))))

;;; ---------------------------------------------------------------------- 
;;;				  Django

(param django-guitar nil)
(param django-guitar-chords nil)

(defun django-guitar (&key (parent QL2))
  (free-orchestra! :node parent)
  (setf django-guitar
	(create-instrument
	 'django-guitar
	 :parent parent
	 :transient t
	 :program-change-hook
	 (keyswitch 'django-guitar
		    '((short . (c2  "short"     :range (e2 d6)))
		      (long  . (cs2 "long"      :range (e2 d6))))))))

(defun django-guitar-chords (&key (parent QL2))
  (free-orchestra! :node parent)
  (setf django-guitar-chords
	(create-instrument
	 'django-guitar-chords
	 :parent parent
	 :transient t
	 :program-change-hook
	 (keyswitch 'django-guitar-chords
		    '((maj           . (c2  "major"     :range (e2 d6)))
		      (min           . (cs2 "minor"     :range (e2 d6)))
		      (dom7          . (d2  "dom7"      :range (e2 d6)))
		      (maj9          . (ds2 "maj9"      :range (e2 d6)))
		      (dim7          . (e2  "dim7"      :range (e2 d6)))
		      (maj6          . (f2  "maj6"      :range (e2 d6)))
		      (min7          . (fs2 "min7"      :range (e2 d6)))
		      (maj7          . (g2  "maj7"      :range (e2 d6)))
		      (seven-flat-5  . (gs2 "seven-flat-5" :range (e2 d6))))))))


;;; ---------------------------------------------------------------------- 
;;;				 Flamenco

(param flamenco-guitar nil)
(param flamenco-guitar-chords nil)

;; note range (e2 a4)
;; noise range (c5 d6)
;;
(defun flamenco-guitar (&key (parent QL2))
  (free-orchestra! :node parent)
  (create-instrument
   'flamenco-guitar
   :parent parent
   :transient t
   :program-change-hook
   (keyswitch 'flamenco-guitar
	      '((sus         . (c2  "sus"         :range (e2 a5)))
		(sus-bridge  . (cs2 "sus-bridge"  :range (e2 a5)))
		(sus-tremolo . (d2  "sus-tremolo" :range (e2 d5)))  ;; Tremolo above d5 only
		(taps        . (ds2 "taps"        :range (d3 c7)))))))

;; noise range (c5 d6)
;;
(defun flamenco-guitar-chords (&key (parent QL2))
  (free-orchestra! :node parent)
  (create-instrument
   'flamenco-guitar-chords
   :parent parent
   :transient t
   :program-change-hook
   (keyswitch 'flamenco-guitar-chords
	      '((fast-major    . (c2  "fast-major"     :range (e1 e4)))
		(fast-minor    . (cs2 "fast-minor"     :range (e1 e4)))
		(arpeg-major   . (d2  "arpeg-major"    :range (e1 ds4)))
		(arpeg-minor   . (ds2 "arpeg-minor"    :range (e1 ds4)))
		(trem-major    . (e2  "trem-major"     :range (e1 e4)))
		(trem-minor    . (f2  "trem-minor"     :range (e1 e4)))
		(roll-major    . (fs2 "roll-major"     :range (e1 ds3)))
		(roll-minor    . (g2  "roll-minor"     :range (e2 ds3)))
		(double-major  . (gs2 "double-major"   :range (e1 e3)))
		(double-minor  . (a2  "double-minor"   :range (e1 e3)))
		(sus-major     . (as2 "sus-major"      :range (e1 e3)))
		(sus-minor     . (b2  "sus-minor"      :range (e1 e3)))
		(sus-vs-maj    . (c3  "sus-vs-maj"     :range (e1 e3)))
		(sus-vs-min    . (cs3 "sus-vs-min"     :range (e1 e3)))
		(stacato-maj   . (d3  "stacato-maj"    :range (e1 e3)))
		(stacato-min   . (ds3 "stacato-min"    :range (e1 e3)))))))

;;; ---------------------------------------------------------------------- 
;;;			      Spanish Guitar

(param spanish-guitar nil)

(defun spanish-guitar (&key (parent QL2))
  (free-orchestra! :node parent)
  (setf spanish-guitar
	(create-instrument
	 'spanish-guitar
	 :parent parent
	 :transient t
	 :program-change-hook
	 (keyswitch 'spanish-guitar
		    '((sus               . (c2  "sus"              :range (d2  a5)))
		      (strum             . (CS2 "strum"            :range (e2  a5)))
		      (strum-fast        . (D2  "srtum-fast"       :range (e2  a5)))
		      (half-slide        . (DS2 "half-slide"       :range (d3  e5)))
		      (whole-slide       . (E2  "whole-slide"      :range (d3  e5)))
		      (half-slide-up     . (F2  "half-slide-up"    :range (f3  a5)))
		      (half-slide-down   . (fs2 "half-slide-down"  :range (e2  a5)))
		      (staccato-mute-pop . (g2  "stccato-mute-pop" :range (e2  a5)))
		      (staccato          . (gs2 "staccato"         :range (e2  a5)))
		      (harmonics         . (a2  "harmonics"        :range (e3  e6)))
		      (legato-sus        . (as2 "legato-sus"       :range (d2  a5)))
		      (noises            . (b2  "noises"           :range (e2  as3)))
		      (neck-slides       . (c3  "neck-slides"      :range (e2  a5))) ;; Range in 4 discreet areas
		      (pitchless-chugs   . (cs3  "pitchless-chugs" :range (e2  g3)))
		      (taps              . (d3  "taps"             :range (e2  b4))))))))
       
