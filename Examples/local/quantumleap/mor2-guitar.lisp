;;;; cyco local quantumleap mor2-guitar
;;;;

(in-package :cyco)

(param --mor2-baritone-keyswith nil)
(param --mor2-baritone-rhythm-keyswith nil)
(param --mor2-carvin-bridge-keyswith nil)
(param --mor2-carvin-bridge-rhythm-keyswith nil)
(param --mor2-carvin-neck-keyswith nil)
(param --mor2-carvin-neck-rhythm-keyswith nil)
(param --mor2-jaguar-keyswith nil)
(param --mor2-jaguar-rhythm-keyswith nil)
(param --mor2-thinline-keyswith nil)
(param --mor2-thinline-rhythm-keyswith nil)
(param --mor2-lespaul-keyswith nil)
(param --mor2-lespaul-rhythm-keyswith nil)
(param --mor2-schecter-keyswith nil)
(param --mor2-schecter-rhythm-keyswith nil)

;; -------------------------------------
;; Baritone lead
;;
(param --mor2-baritone-keyswitch 
       (keyswitch 'mor2-baritone-keyswitch
		  '((SUS       . (C2  "Sustain RR" :range (b2 b6)))
		    (HAMMER    . (CS2 "Hammer-n-pull linked" :range (b2 b6)))
		    (SLIDE     . (D2  "Slide linked" :range (b2 b6)))
		    (HARM      . (DS2 "Harmonics" :range (b3 e7)))
		    (STACCATO  . (E2  "Staccato RR" :range (b3 b6)))
		    (PALM      . (F2  "Palm RR" :range (b3 b6)))
		    (TREM      . (FS2 "Tremolo" :range (b3 fs4))))))

;; -------------------------------------
;; Baritone rhythm
;;
(param --mor2-baritone-rhythm-keyswitch 
       (keyswitch 'mor2-baritone-rhythm-keyswitch
		  '((SUS       . (C2  "PC Sustain"   :range (b2 b4)))
		    (SUS2      . (CS2 "PC Sustain"   :range (b2 b4)))
		    (SUS3      . (D2  "PC Down up"   :range (b2 b4)))
		    (HAMMER    . (DS2 "PC Hammer linked" :range (b2 b4)))
		    (SLIDE     . (E2  "PC Slide  linked" :range (b2 b4)))
		    (SLIDE2    . (F2  "PC Slide fast linked" :range (b2 b4)))
		    (SHORT     . (FS2 "PC Short RR" :range (b2 a4)))
		    (DEATH     . (G2  "PC death" :range (b2 c4)))
		    (DEATH2    . (GS2 "PC death long" :range (b2 c4)))
		    (DEATH3    . (A2  "PC death slow" :range (b2 c4)))
		    (PALM      . (AS2 "PC palm slow RR" :range (b2 b4)))
		    (PALM2     . (B2  "PC palm medium RR" :range (b2 b4)))
		    (STACCATO  . (C3  "PC Staccato palm fast RR" :range (b2 b4)))
		    (STACCATO2 . (CS3 "PC Staccato fast RR" :range (b2 b4)))
		    (PERF      . (D3  "PC Perf palm" :range (b2 b4)))
		    (CHUG      . (DS3 "Chugs short" :range (b2 f6)))
		    (CHUG2     . (E3  "Chugs FX" :range (b2 as5)))
		    (NOISE     . (F3  "Random FX" :range (b2 b4)))
		    (SCRAPE    . (FS3 "Scrapes" :range (b2 fs6))))))

;; -------------------------------------
;; Carvin bridge lead
;;
(param --mor2-carvin-bridge-keyswitch 
       (keyswitch 'mor2-carvin-bridge-keyswitch
		  '((SUS       . (C2  "Sustain RR" :range (c3 c7)))
		    (VIB       . (CS2 "Sustain medium vibrato RR" :range (c3 c7)))
		    (VIB2      . (D2  "Sustain hard vibrato RR" :range (c3 c7)))
		    (HAMMER    . (DS2 "Hammer-n-pull linked" :range (c3 c7)))
		    (SLIDE     . (E2  "Slide linked" :range (c3 c7)))
		    (SCREAM    . (F2  "Scream" :range (c5 c7)))
		    (HARM      . (FS2 "Harmonics" :range (c4 c8)))
		    (PINCH     . (G2  "Pinch harmonics" :range (e4 gs5)))
		    (SCRAPE    . (GS2 "Scrapes" :range (c5 c7)))
		    (STACCATO  . (A2  "Staccato RR" :range (c3 c7)))
		    (PALM      . (AS2 "Staccato palm RR" :range (c3 c7)))
		    (MUTE      . (B2  "Staccato mute RR" :range (b3 c7)))
		    (PERF      . (C3  "Perf" :range (c3 c7)))
		    (PERF2     . (CS3 "Perf mute" :range (c3 c7)))
		    (SCRAPE2   . (D3  "Scrapes 2" :range (c3 c8)))
		    (SCRAPE3   . (DS3 "Scrapes 3" :range (c3 c8)))
		    (SCREAM2   . (E3  "Scream fx" :range (b4 a5)))  
		    (CLUSTER   . (FS3 "Clusters" :range (c3 fs3))))))

;; -------------------------------------
;; Carvin bridge rhythm 
;;
(param --mor2-carvin-bridge-rhythm-keyswitch 
       (keyswitch 'mor2-carvin-bridge-rhythm-keyswitch
		  '((SUS       . (C2  "PC down" :range (c3 c5)))
		    (SUS2      . (CS2 "PC Up"   :range (c3 c5)))
		    (SUS3      . (D2  "PC Double RR" :range (c3 c5)))
		    (SLIDE     . (DS2 "PC Slide up/down linked" :range (c3 c5)))
		    (SLIDE2    . (E2  "PC Slide up/down linked" :range (c3 c5)))
		    (PALM      . (F2  "PC Palm medium RR" :range (c3 g4)))
		    (PALM2     . (FS2 "PC Palm short RR" :range (c3 c5))))))

;; -------------------------------------
;; Carvin neck lead
;;
(param --mor2-carvin-neck-keyswitch 
       (keyswitch 'mor2-carvin-neck-keyswitch
		  '((SUS      . (C2  "Sustain RR" :range (c3 c5)))
		    (VIB      . (CS2 "Sustain medium vibrato RR" :range (c3 c5)))
		    (VIB2     . (D2  "Sustain exp vibrato RR" :range (cs3 c5)))
		    (HAMMER   . (DS2 "Hammer-n-pull (linked)" :range (c3 c5)))
		    (SLIDE    . (E2  "Slide fast (linked)" :range (c3 c5)))
		    (SLIDE2   . (F2  "Slide up" :range (cs3 c5)))
		    (SCREAM   . (FS2 "Scream" :range (c5 a6)))
		    (HARM     . (G2  "Harmonics" :range (c4 d7)))
		    (PINCH    . (GS2 "Pinch Harmonics FX" :range (c3 g6)))
		    (FLUTTER  . (B2  "Flutter Arpeggio" :range (c3 f4)))
		    (STACCATO . (C3  "Staccato RR" :range (c3 c7)))
		    (PALM     . (CS3 "Palm Staccato RR" :range (c3 e4))))))

;; -------------------------------------
;; Carvin neck rhythm
;;
(param --mor2-carvin-neck-rhythm-keyswitch 
       (keyswitch 'mor2-carvin-neck-rhythm-keyswitch
		  '((PC      . (C2  "Power Chord 1 Down" :range (c3 c5)))
		    (PC2     . (CS2 "Power Chord 3 Down" :range (c3 c5)))
		    (PC3     . (D2  "Power Chord 3 RR" :range (c3 c5)))
		    (HAMMER  . (DS2 "Power Chord hammer-n-pull (linked)" :range (c3 c5)))
		    (SLIDE   . (E2  "Power Chord Fast Slide (linked)" :range (c3 c5)))
		    (SLIDE2  . (F2  "Power Chord Slow Slide (linked)" :range (c3 c5)))
		    (PALM    . (FS2 "Power Chord Palm RR" :range (c3 c5)))
		    (PALM2   . (G2  "Perf Palm Long" :range (c3 c5)))
		    (PALM3   . (GS2 "Perf Palm Medium" :range (c3 c5)))
		    (PALM4   . (A2  "Perf Palm Short" :range (c3 c5)))
		    (CHUG    . (AS2 "Chugs 1" :range (c3 c5)))
		    (CHUG2   . (B2  "Chugs 2" :range (g2 c8))))))

;; -------------------------------------
;; Fender Jaguar Lead
;;
(param --mor2-jaguar-keyswitch 
       (keyswitch 'mor2-jaguar-keyswitch
		  '((SUS     . (C2  "Sustain RR" :range (e3 c7)))
		    (VIB     . (CS2 "Sustain vibrato RR" :range (e3 c7)))
		    (3SEC    . (D2  "Sustain 3 second RR" :range (e3 c7)))
		    (3SECVIB . (DS2 "Sustain 3 seconds vibrato RR" :range (e3 c7)))
		    (LONG    . (E2  "Strum long position 1" :range (e3 g6)))
		    (LONG2   . (F2  "Strum long position 2" :range (e3 g6)))
		    (HAMMER  . (FS2 "Hammer-n-pull (linked)" :range (e3 c7)))
		    (SLIDE   . (G2  "Slide up & down (linked)" :range (e3 c7)))
		    (SLIDE2  . (GS2 "Slide up vibrato HT" :range (e3 c7)))
		    (SLIDE3  . (A2  "Slide up vibrato WT" :range (e3 c7)))
		    (SCRAPE  . (AS2 "Scrape long vm" :range (e3 c7)))
		    (SCRAPE2 . (B2  "Scrape short slide down Vm" :range (g3 c7)))
		    (TREM    . (C3  "Tremolo" :range (e3 c7)))
		    (TREM2   . (CS3 "Tremolo mute" :range (e3 c7)))
		    (STRUM   . (DS3 "Strum short RR" :range (e3 g6)))
		    (QUARTER . (E3  "Quarter note RR" :range (e3 c7)))
		    (FALL    . (F3  "Octave fall" :range (fs4 c7))))))

;; -------------------------------------
;; Fender Jaguar Strummer
;;
(param --mor2-jaguar-rhythm-keyswitch 
       (keyswitch 'mor2-jaguar-rhythm-keyswitch
		  '((LONG    . (C2  "Long position 1 RR" :range (e3 g5)))
		    (LONG2   . (CS2 "Long position 2 RR" :range (e3 g5)))
		    (SHORT   . (D2  "Short RR" :range (e3 g5))))))

;; -------------------------------------
;; Fender Telecaster thinline lead
;;
(param --mor2-thinline-keyswitch 
       (keyswitch 'mor2-thinline-keyswitch
		  '((SUS      . (C2  "Sustain" :range (e3 cs7)))
		    (VIB      . (CS2 "Sustain vibrato" :range (e3 cs7)))
		    (VIB2     . (D2  "Vibrato accent" :range (e3 cs7)))
		    (POS1     . (DS2 "Rhythm pos 1 RR" :range (e3 cs7)))
		    (POS2     . (E2  "Rhythm pos 2 RR" :range (e3 d6)))
		    (HAMMER   . (F2  "Hammer-n-pull (linked)" :range (e3 c6)))
		    (SLIDE    . (FS2 "Slide up n down (linked)" :range (e3 c6)))
		    (SCREAM   . (G2  "Scream" :range (a3 ds6)))
		    (BEND     . (GS2 "Bend down fast" :range (g3 cs6)))
		    (FALL     . (A2  "Fall down" :range (g3 cs6)))
		    (FALL2    . (AS2 "Fall up" :range (f3 c6)))
		    (HARM     . (C2  "Harmonics" :range (e4 a4 e5 a5 e6 a6)))
		    (STACCATO . (C3  "Staccato RR" :range (e3 cs6)))
		    (SCRAPE   . (CS3 "Scrapes" :range (e3 d6)))
		    (NOISE    . (D3  "Noise 1" :range (e3 c8)))
		    (NOISE2   . (DS3 "Noise 2" :range (e3 c8))))))

;; -------------------------------------
;; Fender Telecaster thinline rhythm
;;
(param --mor2-thinline-rhythm-keyswitch 
       (keyswitch 'mor2-thinline-rhythm-keyswitch
		  '((LONG    . (C2  "Pos 1 RR" :range (e3 d6)))
		    (LONG2   . (CS2 "Pos 2 RR" :range (e3 d6)))
		    (XX      . (C0  "Noise (not switched)" :range (f5 c8))))))

;; -------------------------------------
;; Gibson Les Paul 7-string lead
;;
(param --mor2-lespaul-keyswitch 
       (keyswitch 'mor2-lespaul-keyswitch
		  '((SUS       . (A1  "Sustain RR" :range (a2 d7)))
		    (VIB       . (AS1 "Vibrato medium RR" :range (as2 d7)))
		    (VIB2      . (B1  "Vibrato hard RR" :range (as2 d7)))
		    (HAMMER    . (C2  "Hammer-n-pull medium vibrato linked-a" :range (as2 d7)))
		    (SLIDE     . (CS2 "Slide up/down medium vibrato linked-a" :range (as2 d7)))
		    (HAMMER2   . (D2  "Hammer-n-pull hard vibrato linked-b" :range (as2 d7)))
		    (SLIDE2    . (DS2 "Slide up/down slow linked-b" :range (as2 d7)))
		    (BEND      . (E2  "Bend up" :range (b2 ds7)))
		    (BEND2     . (F2  "Bend down" :range (b2 ds7)))
		    (SCREAM    . (FS2 "Scream bend up" :range (b4 b6)))
		    (SCREAM2   . (G2  "Scream fast bend down" :range (gs4 as6)))
		    (SCREAM3   . (GS2 "Scream fast bend up" :range (a4 as6)))
		    (SCREAM4   . (A2  "Scream vibrato bend up" :range (b4 b6)))
		    (PINCH     . (AS2 "Pinch harmonics" :range (g3 c6)))
		    (STACCATO  . (B2  "Staccato RR" :range (g2 d7)))
		    (MUTE      . (C3  "Staccato mute RR" :range (g2 d7)))
		    (PERF      . (CS3 "Perf fast mute" :range (g2 d7)))
		    (PERF2     . (D3  "Perf fast" :range (g2 d7)))
		    (NOISE     . (DS3 "Noise fx" :range (g2 d6)))
		    (NOISE2    . (E3  "Random fx" :range (a3 c8))))))

;; -------------------------------------
;; Gibson Les Paul 7-string rhythm
;;
(param --mor2-lespaul-rhythm-keyswitch 
       (keyswitch 'mor2-lespaul-rhythm-keyswitch
		  '((SUS       . (A1  "PC Long Faster RR" :range (a2 a4)))
		    (SUS2      . (AS1 "PC Long slower RR" :range (a2 a4)))
		    (DEATH     . (B1  "PC long death RR" :range (a2 b3)))
		    (HAMMER    . (C2  "PC hammer-n-pull linked" :range (a2 a4)))
		    (SLIDE     . (CS2 "PC slide up/down linked" :range (a2 a4)))
		    (SLIDE2    . (D2  "PC slide up/down slow linked" :range (a2 a4)))
		    (STACCATO  . (DS2 "PC staccato RR" :range (a2 a4)))
		    (PALM      . (E2  "PC palm mute long RR" :range (a2 a4)))
		    (PALM2     . (F2  "PC palm mute medium RR" :range (a2 a4)))
		    (PALM3     . (FS2 "PC palm mute short RR" :range (a2 a4)))
		    (CHUG      . (G2  "PC Chug long RR" :range (a2 as3)))
		    (PERF      . (A2  "Perf" :range (a2 a4)))
		    (PERF2     . (AS2 "Perf medium" :range (a2 a4)))
		    (CHUG      . (C3  "PC Chug RR" :range (a2 a3)))
		    (CHUG2     . (DS3 "PC Chug hammer" :range (g2 c8))))))

;; -------------------------------------
;; Schecter 7-string lead
;;
(param --mor2-schecter-keyswitch 
       (keyswitch 'mor2-schecter-keyswitch
		  '((SUS       . (C2  "Sus" :range (b2 d7)))
		    (VIB       . (CS2 "Sus vibrato rr" :range (b2 d7)))
		    (VIB2      . (D2  "Molto vibrato" :range (b2 d7)))
		    (HAMMER    . (DS2 "Hammer-n-pull linked" :range (b2 d7)))
		    (SLIDE     . (E2  "Slide up/down linked" :range (b2 d7)))
		    (SCRAPE    . (F2  "Scrape vibrato" :range (b2 d7)))
		    (SCREAM    . (FS2 "Scream fall vibrato" :range (b2 d7)))
		    (PINCH     . (G2  "Pinch harmonic" :range (b2 d7)))
		    (HARM      . (GS2 "Harmonics" :range (d4 e7)))
		    (SCREAM2   . (A2  "Scream fall" :range (b2 f4)))
		    (STACCATO  . (AS2 "Staccato mute RR" :range (b2 d7))))))

;; -------------------------------------
;; Schecter 7-string rhythm
;;
(param --mor2-schecter-rhythm-keyswitch 
       (keyswitch 'mor2-schecter-rhythm-keyswitch
		  '((LONG      . (C2  "PC long rr" :range (b2 fs4)))
		    (SHORT     . (CS2 "PC short rr" :range (b2 fs4)))
		    (DEATH     . (D2  "PC death long RR" :range (b2 fs4)))
		    (DEATH2    . (DS2 "PC death long slow" :range (b2 fs4)))
		    (LONG2     . (E2  "PC long slow RR" :range (b2 fs4)))
		    (PALM      . (F2  "Palm mute long RR" :range (b2 fs4)))
		    (PALM2     . (FS2 "Palm mute medium RR" :range (b2 fs4)))
		    (PALM3     . (G2  "Palm mute short RR" :range (b2 fs4)))
		    (REPS      . (GS2 "Reps fast RR" :range (b2 fs4)))
		    (PERF      . (A2  "Perf fast" :range (b2 fs4)))
		    (HAMMER    . (AS2 "Hammer-n-pull linked" :range (b2 g4)))
		    (SLIDE     . (B2  "Slide up/down linked" :range (b2 g4))))))

(param mor2-guitar (create-instrument
		    'mor2-guitar
		    :parent QL2
		    :transient nil))

(param baritone nil)
(param baritone-rhythm nil)
(param carvin-bridge nil)
(param carvin-bridge-rhythm nil)
(param carvin-neck nil)
(param carvin-neck-rhythm nil)
(param jaguar nil)
(param jaguar-rhythm nil)
(param thinline nil)
(param thinline-rhythm nil)
(param lespaul nil)
(param lespaul-rhythm nil)
(param schecter nil)
(param schecter-rhythm nil)

(defun baritone (&key (parent mor2-guitar))
  (free-orchestra! :node parent)
  (setf baritone (create-instrument
		  'baritone
		  :parent parent
		  :transient t
		  :program-change-hook --mor2-baritone-keyswitch)))

(defun baritone-rhythm (&key (parent mor2-guitar))
  (free-orchestra! :node parent)
  (setf baritone-rhythm (create-instrument
			 'baritone-rhythm
			 :parent parent
			 :transient t
			 :program-change-hook --mor2-baritone-rhythm-keyswitch)))

(defun carvin-bridge (&key (parent mor2-guitar))
  (free-orchestra! :node parent)
  (setf carvin-bridge (create-instrument
		       'carvin-bridge
		       :parent parent
		       :transient t
		       :program-change-hook --mor2-carvin-bridge-keyswitch)))

(defun carvin-bridge-rhythm (&key (parent mor2-guitar))
  (free-orchestra! :node parent)
  (setf carvin-bridge-rhythm (create-instrument
			      'carvin-bridge-rhythm
			      :parent parent
			      :transient t
			      :program-change-hook --mor2-carvin-bridge-rhythm-keyswitch)))

(defun carvin-neck (&key (parent mor2-guitar))
  (free-orchestra! :node parent)
  (setf carvin-neck (create-instrument
		     'carvin-neck
		     :parent parent
		     :transient t
		     :program-change-hook --mor2-carvin-neck-keyswitch)))

(defun carvin-neck-rhythm (&key (parent mor2-guitar))
  (free-orchestra! :node parent)
  (setf carvin-neck-rhythm (create-instrument
			    'carvin-neck-rhythm
			    :parent parent
			    :transient t
			    :program-change-hook --mor2-carvin-neck-rhythm-keyswitch)))

(defun jaguar (&key (parent mor2-guitar))
  (free-orchestra! :node parent)
  (setf jaguar (create-instrument
		'jaguar
		:parent parent
		:transient t
		:program-change-hook --mor2-jaguar-keyswitch)))

(defun jaguar-rhythm (&key (parent mor2-guitar))
  (free-orchestra! :node parent)
  (setf jaguar-rhythm (create-instrument
		       'jaguar-rhythm
		       :parent parent
		       :transient t
		       :program-change-hook --mor2-jaguar-rhythm-keyswitch)))

(defun thinline (&key (parent mor2-guitar))
  (free-orchestra! :node parent)
  (setf thinline (create-instrument
		  'thinline
		  :parent parent
		  :transient t
		  :program-change-hook --mor2-thinline-keyswitch)))

(defun thinline-rhythm (&key (parent mor2-guitar))
  (free-orchestra! :node parent)
  (setf thinline-rhythm (create-instrument
			 'thinline-rhythm
			 :parent parent
			 :transient t
			 :program-change-hook --mor2-thinline-rhythm-keyswitch)))

(defun lespaul (&key (parent mor2-guitar))
  (free-orchestra! :node parent)
  (setf lespaul (create-instrument
		 'lespaul
		 :parent parent
		 :transient t
		 :program-change-hook --mor2-lespaul-keyswitch)))

(defun lespaul-rhythm (&key (parent mor2-guitar))
  (free-orchestra! :node parent)
  (setf lespaul-rhythm (create-instrument
			'lespaul-rhythm
			:parent parent
			:transient t
			:program-change-hook --mor2-lespaul-rhythm-keyswitch)))

(defun schecter (&key (parent mor2-guitar))
  (free-orchestra! :node parent)
  (setf schecter (create-instrument
		  'schecter
		  :parent parent
		  :transient t
		  :program-change-hook --mor2-schecter-keyswitch)))

(defun schecter-rhythm (&key (parent mor2-guitar))
  (free-orchestra! :node parent)
  (setf schecter-rhythm (create-instrument
			 'schecter-rhythm
			 :parent parent
			 :transient t
			 :program-change-hook --mor2-schecter-rhythm-keyswitch)))


