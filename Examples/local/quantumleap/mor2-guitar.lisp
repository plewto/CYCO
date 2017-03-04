;;;; quantumleap mor2-guitar
;;;;

(in-package :cyco)

(let ((baritone-keyswitch 
       (keyswitch 'baritone
		  '((SUS       C1  "Sustain RR" :range (b1 b5))
		    (HAMMER    CS1 "Hammer-n-pull linked" :range (b1 b5))
		    (SLIDE     D1  "Slide linked" :range (b1 b5))
		    (HARM      DS1 "Harmonics" :range (b2 e6))
		    (STACCATO  E1  "Staccato RR" :range (b2 b5))
		    (PALM      F1  "Palm RR" :range (b2 b5))
		    (TREM      FS1 "Tremolo" :range (b2 fs3)))))
      (baritone-rhythm-keyswitch 
       (keyswitch 'baritone-rhythm
		  '((SUS       C1  "PC Sustain"   :range (b1 b3))
		    (SUS2      CS1 "PC Sustain"   :range (b1 b3))
		    (SUS3      D1  "PC Down up"   :range (b1 b3))
		    (HAMMER    DS1 "PC Hammer linked" :range (b1 b3))
		    (SLIDE     E1  "PC Slide  linked" :range (b1 b3))
		    (SLIDE2    F1  "PC Slide fast linked" :range (b1 b3))
		    (SHORT     FS1 "PC Short RR" :range (b1 a3))
		    (DEATH     G1  "PC death" :range (b1 c3))
		    (DEATH2    GS1 "PC death long" :range (b1 c3))
		    (DEATH3    A1  "PC death slow" :range (b1 c3))
		    (PALM      AS1 "PC palm slow RR" :range (b1 b3))
		    (PALM2     B1  "PC palm medium RR" :range (b1 b3))
		    (STACCATO  C2  "PC Staccato palm fast RR" :range (b1 b3))
		    (STACCATO2 CS2 "PC Staccato fast RR" :range (b1 b3))
		    (PERF      D2  "PC Perf palm" :range (b1 b3))
		    (CHUG      DS2 "Chugs short" :range (b1 f5))
		    (CHUG2     E2  "Chugs FX" :range (b1 as4))
		    (NOISE     F2  "Random FX" :range (b1 b3))
		    (SCRAPE    FS2 "Scrapes" :range (b1 fs5)))))
      (carvin-bridge-keyswitch 
       (keyswitch 'carvin-bridge
		  '((SUS       C1  "Sustain RR" :range (c2 c6))
		    (VIB       CS1 "Sustain medium vibrato RR" :range (c2 c6))
		    (VIB2      D1  "Sustain hard vibrato RR" :range (c2 c6))
		    (HAMMER    DS1 "Hammer-n-pull linked" :range (c2 c6))
		    (SLIDE     E1  "Slide linked" :range (c2 c6))
		    (SCREAM    F1  "Scream" :range (c4 c6))
		    (HARM      FS1 "Harmonics" :range (c3 c7))
		    (PINCH     G1  "Pinch harmonics" :range (e3 gs4))
		    (SCRAPE    GS1 "Scrapes" :range (c4 c6))
		    (STACCATO  A1  "Staccato RR" :range (c2 c6))
		    (PALM      AS1 "Staccato palm RR" :range (c2 c6))
		    (MUTE      B1  "Staccato mute RR" :range (b2 c6))
		    (PERF      C2  "Perf" :range (c2 c6))
		    (PERF2     CS2 "Perf mute" :range (c2 c6))
		    (SCRAPE2   D2  "Scrapes 2" :range (c2 c7))
		    (SCRAPE3   DS2 "Scrapes 3" :range (c2 c7))
		    (SCREAM2   E2  "Scream fx" :range (b3 a4))  
		    (CLUSTER   FS2 "Clusters" :range (c2 fs2)))))
      (carvin-bridge-rhythm-keyswitch 
       (keyswitch 'carvin-bridge-rhythm
		   '((SUS       C1  "PC down" :range (c2 c4))
		    (SUS2      CS1 "PC Up"   :range (c2 c4))
		    (SUS3      D1  "PC Double RR" :range (c2 c4))
		    (SLIDE     DS1 "PC Slide up/down linked" :range (c2 c4))
		    (SLIDE2    E1  "PC Slide up/down linked" :range (c2 c4))
		    (PALM      F1  "PC Palm medium RR" :range (c2 g3))
		    (PALM2     FS1 "PC Palm short RR" :range (c2 c4)))))
      (carvin-neck-keyswitch 
       (keyswitch 'carvin-neck
		  '((SUS      C1  "Sustain RR" :range (c2 c4))
		    (VIB      CS1 "Sustain medium vibrato RR" :range (c2 c4))
		    (VIB2     D1  "Sustain exp vibrato RR" :range (cs2 c4))
		    (HAMMER   DS1 "Hammer-n-pull (linked)" :range (c2 c4))
		    (SLIDE    E1  "Slide fast (linked)" :range (c2 c4))
		    (SLIDE2   F1  "Slide up" :range (cs2 c4))
		    (SCREAM   FS1 "Scream" :range (c4 a5))
		    (HARM     G1  "Harmonics" :range (c3 d6))
		    (PINCH    GS1 "Pinch Harmonics FX" :range (c2 g5))
		    (FLUTTER  B1  "Flutter Arpeggio" :range (c2 f3))
		    (STACCATO C2  "Staccato RR" :range (c2 c6))
		    (PALM     CS2 "Palm Staccato RR" :range (c2 e3)))))
      (carvin-neck-rhythm-keyswitch 
       (keyswitch 'carvin-neck-rhythm
		  '((PC      C1  "Power Chord 1 Down" :range (c2 c4))
		    (PC2     CS1 "Power Chord 3 Down" :range (c2 c4))
		    (PC3     D1  "Power Chord 3 RR" :range (c2 c4))
		    (HAMMER  DS1 "Power Chord hammer-n-pull (linked)" :range (c2 c4))
		    (SLIDE   E1  "Power Chord Fast Slide (linked)" :range (c2 c4))
		    (SLIDE2  F1  "Power Chord Slow Slide (linked)" :range (c2 c4))
		    (PALM    FS1 "Power Chord Palm RR" :range (c2 c4))
		    (PALM2   G1  "Perf Palm Long" :range (c2 c4))
		    (PALM3   GS1 "Perf Palm Medium" :range (c2 c4))
		    (PALM4   A1  "Perf Palm Short" :range (c2 c4))
		    (CHUG    AS1 "Chugs 1" :range (c2 c4))
		    (CHUG2   B1  "Chugs 2" :range (g1 c7)))))
      (jaguar-keyswitch 
       (keyswitch 'jaguar
		  '((SUS     C1  "Sustain RR" :range (e2 c6))
		    (VIB     CS1 "Sustain vibrato RR" :range (e2 c6))
		    (3SEC    D1  "Sustain 3 second RR" :range (e2 c6))
		    (3SECVIB DS1 "Sustain 3 seconds vibrato RR" :range (e2 c6))
		    (LONG    E1  "Strum long position 1" :range (e2 g5))
		    (LONG2   F1  "Strum long position 2" :range (e2 g5))
		    (HAMMER  FS1 "Hammer-n-pull (linked)" :range (e2 c6))
		    (SLIDE   G1  "Slide up & down (linked)" :range (e2 c6))
		    (SLIDE2  GS1 "Slide up vibrato HT" :range (e2 c6))
		    (SLIDE3  A1  "Slide up vibrato WT" :range (e2 c6))
		    (SCRAPE  AS1 "Scrape long vm" :range (e2 c6))
		    (SCRAPE2 B1  "Scrape short slide down Vm" :range (g2 c6))
		    (TREM    C2  "Tremolo" :range (e2 c6))
		    (TREM2   CS2 "Tremolo mute" :range (e2 c6))
		    (STRUM   DS2 "Strum short RR" :range (e2 g5))
		    (QUARTER E2  "Quarter note RR" :range (e2 c6))
		    (FALL    F2  "Octave fall" :range (fs3 c6)))))
      (jaguar-rhythm-keyswitch 
       (keyswitch 'jaguar-rhythm
		  '((LONG    C1  "Long position 1 RR" :range (e2 g4))
		    (LONG2   CS1 "Long position 2 RR" :range (e2 g4))
		    (SHORT   D1  "Short RR" :range (e2 g4)))))
      (lespaul-keyswitch 
       (keyswitch 'lespaul
		  '((SUS       A0  "Sustain RR" :range (a1 d6))
		    (VIB       AS0 "Vibrato medium RR" :range (as1 d6))
		    (VIB2      B0  "Vibrato hard RR" :range (as1 d6))
		    (HAMMER    C1  "Hammer-n-pull medium vibrato linked-a" :range (as1 d6))
		    (SLIDE     CS1 "Slide up/down medium vibrato linked-a" :range (as1 d6))
		    (HAMMER2   D1  "Hammer-n-pull hard vibrato linked-b" :range (as1 d6))
		    (SLIDE2    DS1 "Slide up/down slow linked-b" :range (as1 d6))
		    (BEND      E1  "Bend up" :range (b1 ds6))
		    (BEND2     F1  "Bend down" :range (b1 ds6))
		    (SCREAM    FS1 "Scream bend up" :range (b3 b5))
		    (SCREAM2   G1  "Scream fast bend down" :range (gs3 as5))
		    (SCREAM3   GS1 "Scream fast bend up" :range (a3 as5))
		    (SCREAM4   A1  "Scream vibrato bend up" :range (b3 b5))
		    (PINCH     AS1 "Pinch harmonics" :range (g2 c5))
		    (STACCATO  B1  "Staccato RR" :range (g1 d6))
		    (MUTE      C2  "Staccato mute RR" :range (g1 d6))
		    (PERF      CS2 "Perf fast mute" :range (g1 d6))
		    (PERF2     D2  "Perf fast" :range (g1 d6))
		    (NOISE     DS2 "Noise fx" :range (g1 d5))
		    (NOISE2    E2  "Random fx" :range (a2 c7)))))
      (lespaul-rhythm-keyswitch 
       (keyswitch 'lespaul-rhythm
		  '((SUS       A0  "PC Long Faster RR" :range (a1 a3))
		    (SUS2      AS0 "PC Long slower RR" :range (a1 a3))
		    (DEATH     B0  "PC long death RR" :range (a1 b2))
		    (HAMMER    C1  "PC hammer-n-pull linked" :range (a1 a3))
		    (SLIDE     CS1 "PC slide up/down linked" :range (a1 a3))
		    (SLIDE2    D1  "PC slide up/down slow linked" :range (a1 a3))
		    (STACCATO  DS1 "PC staccato RR" :range (a1 a3))
		    (PALM      E1  "PC palm mute long RR" :range (a1 a3))
		    (PALM2     F1  "PC palm mute medium RR" :range (a1 a3))
		    (PALM3     FS1 "PC palm mute short RR" :range (a1 a3))
		    (CHUG      G1  "PC Chug long RR" :range (a1 as2))
		    (PERF      A1  "Perf" :range (a1 a3))
		    (PERF2     AS1 "Perf medium" :range (a1 a3))
		    (CHUG      C2  "PC Chug RR" :range (a1 a2))
		    (CHUG2     DS2 "PC Chug hammer" :range (g1 c7)))))
      (schecter-keyswitch 
       (keyswitch 'schecter
		  '((SUS       C1  "Sus" :range (b1 d6))
		    (VIB       CS1 "Sus vibrato rr" :range (b1 d6))
		    (VIB2      D1  "Molto vibrato" :range (b1 d6))
		    (HAMMER    DS1 "Hammer-n-pull linked" :range (b1 d6))
		    (SLIDE     E1  "Slide up/down linked" :range (b1 d6))
		    (SCRAPE    F1  "Scrape vibrato" :range (b1 d6))
		    (SCREAM    FS1 "Scream fall vibrato" :range (b1 d6))
		    (PINCH     G1  "Pinch harmonic" :range (b1 d6))
		    (HARM      GS1 "Harmonics" :range (d3 e6))
		    (SCREAM2   A1  "Scream fall" :range (b1 f3))
		    (STACCATO  AS1 "Staccato mute RR" :range (b1 d6)))))
      (schecter-rhythm-keyswitch 
       (keyswitch 'schecter-rhythm
		  '((LONG      C1  "PC long rr" :range (b1 fs3))
		    (SHORT     CS1 "PC short rr" :range (b1 fs3))
		    (DEATH     D1  "PC death long RR" :range (b1 fs3))
		    (DEATH2    DS1 "PC death long slow" :range (b1 fs3))
		    (LONG2     E1  "PC long slow RR" :range (b1 fs3))
		    (PALM      F1  "Palm mute long RR" :range (b1 fs3))
		    (PALM2     FS1 "Palm mute medium RR" :range (b1 fs3))
		    (PALM3     G1  "Palm mute short RR" :range (b1 fs3))
		    (REPS      GS1 "Reps fast RR" :range (b1 fs3))
		    (PERF      A1  "Perf fast" :range (b1 fs3))
		    (HAMMER    AS1 "Hammer-n-pull linked" :range (b1 g3))
		    (SLIDE     B1  "Slide up/down linked" :range (b1 g3)))))
      (thinline-keyswitch 
       (keyswitch 'thinline
		  '((SUS      C1  "Sustain" :range (e2 cs6))
		    (VIB      CS1 "Sustain vibrato" :range (e2 cs6))
		    (VIB2     D1  "Vibrato accent" :range (e2 cs6))
		    (POS1     DS1 "Rhythm pos 1 RR" :range (e2 cs6))
		    (POS2     E1  "Rhythm pos 2 RR" :range (e2 d5))
		    (HAMMER   F1  "Hammer-n-pull (linked)" :range (e2 c5))
		    (SLIDE    FS1 "Slide up n down (linked)" :range (e2 c5))
		    (SCREAM   G1  "Scream" :range (a2 ds5))
		    (BEND     GS1 "Bend down fast" :range (g2 cs5))
		    (FALL     A1  "Fall down" :range (g2 cs5))
		    (FALL2    AS1 "Fall up" :range (f2 c5))
		    (HARM     C1  "Harmonics" :range (e3 a3 e4 a4 e5 a5))
		    (STACCATO C2  "Staccato RR" :range (e2 cs5))
		    (SCRAPE   CS2 "Scrapes" :range (e2 d5))
		    (NOISE    D2  "Noise 1" :range (e2 c7))
		    (NOISE2   DS2 "Noise 2" :range (e2 c7)))))
      (thinline-rhythm-keyswitch 
       (keyswitch 'thinline-rhythm
		  '((LONG    C1  "Pos 1 RR" :range (e2 d5))
		    (LONG2   CS1 "Pos 2 RR" :range (e2 d5))
		    (XX      C0  "Noise (not switched)" :range (f4 c7)))))
      (remtext (str+ (format nil "Ministry Of Rock II Guitars:~%")
		     (format nil "   Baritone~%")
		     (format nil "   Bartone Rhythm~%")
		     (format nil "   Carvin Bridge~%")
		     (format nil "   Carvin Bridge Rhythm~%")
		     (format nil "   Carvin Neck~%")
		     (format nil "   Carvin Neck Rhythm~%")
		     (format nil "   Jaguar~%")
		     (format nil "   Jaguar Rhythm~%")
		     (format nil "   Lespaul (7 string)~%")
		     (format nil "   Lespaul Rhythm (7 string)~%")
		     (format nil "   Schecter (7 string)~%")
		     (format nil "   Schecter Rhythm (7 string)~%")
		     (format nil "   Thinline (Telacaster hollow body)~%")
		     (format nil "   Thinline Rhythm (Telacaster hollow body~%"))))

  (param mor2-guitar (create-instrument 'mor2-guitar
					:parent ql2
					:transient nil
					:remarks remtext))
  
  (param baritone nil)
  (param carvin-bridge nil)
  (param carvin-neck nil)
  (param jaguar nil)
  (param lespaul nil)
  (param schecter nil)
  (param thinline nil)

  (defun baritone (&key (parent mor2-guitar)(rhythm nil))
    (let ((name (if rhythm 'baritone-rhythm 'baritone))
	  (map (if rhythm baritone-rhythm-keyswith baritone-keyswitch)))
      (setf baritone (create-instrument name
					:parent parent
					:transient t
					:remarks ""
					:program-change-hook map))
      baritone))

  (defun carvin-bridge (&key (parent mor2-guitar)(rhythm nil))
    (let ((name (if rhythm 'carvin-bridge-rhythm 'carvin-bridge))
	  (map (if rhythm carvin-bridge-rhythm-keyswitch carvin-bridge-keyswitch)))
      (setf carvin-bridge (create-instrument name
					     :parent parent
					     :transient t
					     :remarks ""
					     :program-change-hook map))
      carvin-bridge))

  (defun carvin-neck (&key (parent mor2-guitar)(rhythm nil))
    (let ((name (if rhythm 'carvin-neck-rhythm 'carvin-neck))
	  (map (if rhythm carvin-neck-rhythm-keyswitch carvin-neck-keyswitch)))
      (setf carvin-neck (create-instrument name
					   :parent parent
					   :transient t
					   :remarks ""
					   :program-change-hook map))
      carvin-neck))

  (defun jaguar (&key (parent mor2-guitar)(rhythm nil)) 
    (let ((name (if rhythm 'jaguar-rhythm 'jaguar))
	  (map (if rhythm jaguar-rhythm-keyswitch jaguar-keyswitch)))
      (setf jaguar (create-instrument name
				      :parent parent
				      :transient t
				      :remarks ""
				      :program-change-hook map))
      jaguar))

  (defun lespaul (&key (parent mor2-guitar)(rhythm nil))
    (let ((name (if rhythm 'lespaul-rhythm 'lespaul))
	  (map (if rhythm lespaul-rhythm-keyswitch lespaul-keyswitch)))
      (setf lespaul (create-instrument name
				       :parent parent
				       :transient t
				       :remarks "7 String Lespaul"
				       :program-change-hook map))
      lespaul))

  (defun schecter (&key (parent mor2-guitar)(rhythm nil))
    (let ((name (if rhythm 'schecter-rhythm 'schecter))
	  (map (if rhythm schecter-rhythm-keyswitch schecter-keyswitch)))
      (setf schecter (create-instrument name
					:parent parent
					:transient t
					:remarks "7 String Schecter"
					:program-change-hook map))
      schecter))

  (defun thinline (&key (parent mor2-guitar)(rhythm nil)) 
    (let ((name (if rhythm 'thinline-rhythm 'thinline))
	  (map (if rhythm thinline-rhythm-keyswitch thinline-keyswitch)))
      (setf thinline (create-instrument name
					:parent parent
					:transient t
					:remarks "Hollowbody Fender Thinline"
					:program-change-hook map))
      thinline)) )


      
