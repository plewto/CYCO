;;;; cyco local quantumlaep mor2-bass
;;;;

(in-package :cyco)

(param  --mor2-jazzman-keyswitch nil)
(param  --mor2-gibson-eb2-keyswitch nil)
(param  --mor2-hofner-fingered-keyswitch nil)
(param  --mor2-hofner-keyswitch nil)
(param  --mor2-lakland-fingered-keyswitch nil)
(param  --mor2-lakland-keyswitch nil)
(param  --mor2-musicman-keyswitch nil)
(param  --mor2-rickenbacker-keyswitch nil)
(param  --mor2-silvertone-fingered-keyswitch nil)
(param  --mor2-silvertone-keyswitch nil)
(param  --mor2-stingray-fingered-keyswitch nil)
(param  --mor2-stingray-keyswitch nil)

(param --mor2-jazzman-keyswitch
       (keyswitch 'mor2-jazzman
		  '((sus      . (C2  "Sustain"         :range (d2 ds5)))
		    (vib      . (CS2 "Sustain vibrato" :range (d2 ds5)))
		    (slide    . (D2  "Staccato"        :range (d2 ds5)))
		    (slide2   . (DS2 "Slide up slow"   :range (d2 ds5)))
		    (slide3   . (E2  "Slide up medium" :range (d2 cs5)))
		    (slide4   . (G2  "Slide up fast"   :range (d2 d5)))
		    (slide5   . (GS2 "Slide down"      :range (d2 ds5)))
		    (slide6   . (A2  "Slide up octave" :range (d2 ds4)))
		    (hammer   . (AS2 "Hammer"          :range (d2 ds4)))
		    (hammer2  . (B2  "Hammer 2"        :range (d2 a3))))))

;; Gibson EB2 Picked
;;
(param --mor2-gibson-eb2-keyswitch 
       (keyswitch 'mor2-gibson-eb2
		  '((sus      . (C2  "Open sustain RR"  :range (d2 ds5)))
		    (mute     . (CS2 "Mute RR"          :range (d2 e5)))
		    (slide    . (D2  "Slide up sustain" :range (d2 ds5)))
		    (slide2   . (E2  "Slide up"         :range (d2 ds5)))
		    (bend     . (DS2 "Bend up"          :range (d2 ds5)))
		    (trem     . (F2  "Tremolo"          :range (d2 ds5)))
		    (noise    . (FS2 "Noise"            :range (d2 ds5))))))

;; Hofner Fingered
;;
(param --mor2-hofner-fingered-keyswitch 
       (keyswitch 'mor2-hofner-fingered
		  '((sus      . (C2  "Sustain"     :range (d2 f5)))
		    (slide    . (CS2 "Slide up"    :range (d2 f5)))
		    (trem     . (D2  "Tremolo"     :range (d2 cs5)))
		    (noise    . (DS2 "Noise"       :range (d2 g3))))))

;; Hofner Pick
;;
(param --mor2-hofner-keyswitch 
       (keyswitch 'mor2-hofner
		  '((sus      . (C2  "Sustain RR"  :range (d2 f5)))
		    (slide    . (CS2 "Slide up"    :range (d2 f5)))
		    (noise    . (D2  "Noise"       :range (d2 b3))))))

;; Lakland Fingered
;;
(param --mor2-lakland-fingered-keyswitch 
       (keyswitch 'mor2-lakland-fingered
		  '((sus      . (C2  "Open sustain"       :range (b1 f5)))
		    (legato   . (CS2 "Legato"             :range (b1 f5)))
		    (slide    . (D2  "Slide down"         :range (b1 f5)))
		    (slide2   . (E2  "Slide up sustain"   :range (b1 f5)))
		    (brrr     . (FS2 "Brrr"               :range (b1 d5))))))

;; Lakland Pick
;;
(param --mor2-lakland-keyswitch 
       (keyswitch 'mor2-lakland
		  '((open     . (A2  "Open down"          :range (b1 f5)))
		    (open2    . (AS2 "Open up & down split range" :range (b1 as3 b4 b7)))
		    (legato   . (B2  "Legato"             :range (b1 f5)))
		    (slide    . (C3  "Slide down"         :range (b1 f5)))
		    (slide2   . (CS3 "Slide down fast"    :range (b1 f5)))
		    (slide3   . (E3  "Slide up sustain"   :range (b1 f5)))
		    (brrr     . (FS3 "Brrr"               :range (b1 d5)))
		    (mute     . (G3  "Mute down"          :range (b1 f5)))
		    (mute2    . (GS3 "Mute up & down split range" :range (b1 as4 b4 b7))))))

;; Musicman Pick
;; Use with MOR/bass/ks_Musicman
;;
(param --mor2-musicman-keyswitch 
       (keyswitch 'mor2-musicman
		  '((sus      . (A1  "Sustain RR"         :range (b1 b4)))
		    (open     . (AS1 "Open down"          :range (b1 f5)))
		    (legato   . (B1  "Legato"             :range (b1 f5)))
		    (slide    . (CS2 "Slide down fast"    :range (b1 f5)))
		    (slide2   . (E2  "Slide up sustain"   :range (b1 f5)))
		    (slide3   . (A2  "Slide down mute"    :range (b1 f5)))
		    (brrr     . (FS2 "Brrr"               :range (b1 ds5)))
		    (mute     . (G2  "Mute down"          :range (b1 f5)))
		    (mute2    . (GS2 "Mute RR"            :range (b1 as4))))))

;; Rickenbacker Picked
;;
(param --mor2-rickenbacker-keyswitch 
       (keyswitch 'mor2-rickenbacker
		  '((sus      . (C2  "Sustain RR"  :range (d2 ds5)))
		    (slide    . (CS2 "Slide up"    :range (d2 ds5)))
		    (noise    . (D2  "Noise"       :range (d2 c4))))))

;; Silvertone fingered
;;
(param --mor2-silvertone-fingered-keyswitch 
       (keyswitch 'mor2-silvertone-fingered
		  '((sus      . (C2  "Sustain"     :range (d2 d5)))
		    (slide    . (CS2 "Slide up"    :range (d2 d5)))
		    (slide2   . (DS2 "Slide down"  :range (d2 d5)))
		    (bend     . (D2  "Bend up"     :range (d2 d5)))
		    (noise    . (E2  "Noise"       :range (d2 a3))))))

;; Silvertone Picked
;;
(param --mor2-silvertone-keyswitch 
       (keyswitch 'mor2-silvertone
		  '((sus      . (C2  "Sustain RR"  :range (d2 g5)))
		    (legato   . (CS2 "Legato"      :range (d2 g5)))
		    (slide    . (D2  "Slide up"    :range (d2 g5)))
		    (slide2   . (DS2 "Slide down"  :range (d2 g5)))
		    (noise    . (E2  "Noise"       :range (d2 cs5))))))

;; Stingray Fingered 5-string
;;
(param --mor2-stingray-fingered-keyswitch 
       (keyswitch 'mor2-stingray-fingered
		  '((long     . (A1  "Long RR"            :range (a1 ds5)))
		    (short    . (AS1 "Short RR"          :range (a1 ds5)))
		    (mute     . (B1  "Mute RR"            :range (a1 ds5)))
		    (hammer   . (C2  "Hammer-n-pull (linked)" :range (b1 ds5)))
		    (slide    . (CS2 "Slide (linked)"     :range (a1 ds5)))
		    (fall     . (D2  "Fall"               :range (b1 ds5)))
		    (bend     . (E2  "Bend up"            :range (b1 ds5))))))

;; Stingray Picked 5-string
;;
(param --mor2-stingray-keyswitch 
       (keyswitch 'mor2-stingray
		  '((long     . (A1  "Long RR"            :range (a1 ds5)))
		    (short    . (AS1 "Short RR"           :range (a1 ds5)))
		    (mute     . (B1  "Mute RR"            :range (a1 ds5)))
		    (hammer   . (C2  "Hammer-n-pull (linked)" :range (a1 ds5)))
		    (slide    . (CS2 "Slide (linked)"     :range (a1 ds5)))
		    (fall     . (D2  "Fall"               :range (b1 ds5)))
		    (bend     . (E2  "Bend up & down"     :range (a1 ds5)))
		    (rep      . (F2  "Fast rep RR"        :range (a1 ds5)))
		    (perf     . (FS2 "Performance fast"   :range (a1 ds5)))
		    (hammer2  . (G2  "Hammer-n-pull short" :range (a1 ds5))))))

(param mor2-bass (create-instrument
		  'mor2-bass
		  :parent QL1
		  :transient nil))

(param jazzman nil)
(param gibson-eb2 nil)
(param hofner-fingered nil)
(param hofner nil)
(param lakland-fingered nil)
(param lakland nil)
(param musicman nil)
(param rickenbacker nil)
(param silvertone-fingered nil)
(param silvertone nil)
(param stingray-fingered nil)
(param stingray nil)

(defun jazzman (&key (parent mor2-bass))
  (free-orchestra! :node parent)
  (setf jazzman (create-instrument 'jazzman
				   :parent parent
				   :transient t
				   :program-change-hook --mor2-jazzman-keyswitch))
  jazzman)

(defun gibson-eb2 (&key (parent mor2-bass))
  (free-orchestra! :node parent)
  (setf gibson-eb2 (create-instrument 'gibson-eb2
				      :parent parent
				      :transient t
				      :program-change-hook --mor2-gibson-eb2-keyswitch))
  gibson-eb2)

(defun hofner-fingered (&key (parent mor2-bass))
  (free-orchestra! :node parent)
  (setf hofner-fingered (create-instrument 'hofner-fingered
			       :parent parent
			       :transient t
			       :program-change-hook --mor2-hofner-fingered-keyswitch)))

(defun hofner (&key (parent mor2-bass))
  (free-orchestra! :node parent)
  (setf hofner (create-instrument 'hofner
			       :parent parent
			       :transient t
			       :program-change-hook --mor2-hofner-keyswitch)))

(defun lakland-fingered (&key (parent mor2-bass))
  (free-orchestra! :node parent)
  (setf lakland-fingered (create-instrument 'lakland-fingered
			       :parent parent
			       :transient t
			       :program-change-hook --mor2-lakland-fingered-keyswitch)))


(defun lakland (&key (parent mor2-bass))
  (free-orchestra! :node parent)
  (setf lakland (create-instrument 'lakland
			       :parent parent
			       :transient t
			       :program-change-hook --mor2-lakland-keyswitch)))


(defun musicman (&key (parent mor2-bass))
  (free-orchestra! :node parent)
  (setf musicman (create-instrument 'musicman
			       :parent parent
			       :transient t
			       :program-change-hook --mor2-musicman-keyswitch)))


(defun rickenbacker (&key (parent mor2-bass))
  (free-orchestra! :node parent)
  (setf rickenbacker (create-instrument 'rickenbacker
			       :parent parent
			       :transient t
			       :program-change-hook --mor2-rickenbacker-keyswitch)))


(defun silvertone-fingered (&key (parent mor2-bass))
  (free-orchestra! :node parent)
  (setf silvertone-fingered (create-instrument 'silvertone-fingered
			       :parent parent
			       :transient t
			       :program-change-hook --mor2-silvertone-fingered-keyswitch)))


(defun silvertone (&key (parent mor2-bass))
  (free-orchestra! :node parent)
  (setf silvertone (create-instrument 'silvertone
			       :parent parent
			       :transient t
			       :program-change-hook --mor2-silvertone-keyswitch)))


(defun stingray-fingered (&key (parent mor2-bass))
  (free-orchestra! :node parent)
  (setf stingray-fingered (create-instrument 'stingray-fingered
			       :parent parent
			       :transient t
			       :program-change-hook --mor2-stingray-fingered-keyswitch)))


(defun stingray (&key (parent mor2-bass))
  (free-orchestra! :node parent)
  (setf stingray (create-instrument 'stingray
			       :parent parent
			       :transient t
			       :program-change-hook --mor2-stingray-keyswitch)))
