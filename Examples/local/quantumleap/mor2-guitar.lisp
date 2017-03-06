;;;; quantumleap mor2-guitar
;;;;

(in-package :cyco)

(let ((baritone-keyswitch
       (keyswitch 'baritone
		  '((sus         c2  "Sustain down/up RR")
		    (lead-hammer cs2 "Lead hammer on/pull off (link with lead-slide)")
		    (lead-slide  d2  "Lead slide up/down (link with lead-hammer)")
		    (harm        ds2 "Harmonics")
		    (short       e2  "Staccato short RR")
		    (palm        f2  "Staccato palm slow RR")
		    (trem        fs2 "Tremolo"))))

      (baritone-rhythm-keyswitch
       (keyswitch 'baritone-rhythm
		  '((sus    c2  "Pick sustain")
		    (sus2   cs2 "Pick sustain 2")
		    (sus3   d2  "Pick sustain down/up")
		    (hammer ds2 "Hammer on/pull off (link hammer slide slide2)")
		    (slide  e2  "Slide up/down      (link hammer slide slide2)")
		    (slide2 f2  "Slide up/down slow (link hammer slide slide2)")
		    (short  fs2 "Pick short RR")
		    (death  g2  "Death")
		    (death2 gs2 "Death long")
		    (death3 a2  "Death slow")
		    (palm   as2 "Palm slow RR")
		    (palm2  b2  "Palm medium RR")
		    (palm3  c3  "Palm staccato fast RR")
		    (palm4  cs3 "Palm faster RR")
		    (perf   d4  "Performance Palm")
		    (chug   ds4 "Short chugs")
		    (chugs2 e2  "Performance chugs")
		    (fx     f2  "")
		    (scrapes fs2 "Scrapes and noises"))))
      
      (carvin-bridge-keyswitch
       (keyswitch 'carvin-bridge
		  '((sus      c2  "Sustain no-vibrato RR")
		    (vib      cs2 "Sustain med vibrato RR")
		    (vib2     d2  "Sustain deep vibrato RR")
		    (hammer   ds2 "Hammer on/Pull off (link hammer slide)")
		    (slide    e2  "Slide up/down      (link hammer slide)")
		    (scream   f2  "")
		    (harm     fs2 "Harmonics")
		    (pinch    g2  "Pinch harmonics")
		    (scrapes  gs2 "")
		    (steck8va a2  "Staccato drop octave RR")
		    (palm     as2 "Palm RR")
		    (steck    b2  "Staccato mute RR")
		    (perf     c2  "Performance")
		    (perf2    cs2 "Performance 2")
		    (scrapes  d2  "Scrape effects")
		    (scrapes2 ds2 "Scrape effects")
		    (fx-screem e2  "Effects scream")
		    (fx-single f2  "Effects single notes")
		    (clusters  fs2 "Effects clusters"))))

      (carvin-bridge-rhythm-keyswitch
       (keyswitch 'carvin-bridge-rhythm
		  '((down   c2  "Pick down")
		    (up     cs2 "Pick up")
		    (double d2  "Pick double (sustain) RR")
		    (slide  ds2 "Slide up/down (link slide slide2)")
		    (slide2 e2  "Slide up/down (link slide slide2)")
		    (palm   f2  "Palm medium RR")
		    (palm2  fs2 "Palm short RR")
		    (efx    g2  "Rhythmic effects"))))
		    
      (carvin-neck-keyswitch
       (keyswitch 'carvin-neck
		  '((sus      c2  "Sustain no-vibrato RR")
		    (vib      cs2 "Sustain med vibrato RR")
		    (vib2     d2  "Sustain deep vibrato RR")
		    (hammer   ds2 "Hammer on/Pull off (link hammer slide)")
		    (slide    e2  "Slide up/down      (link hammer slide)")
		    (scream   f2  "")
		    (harm     fs2 "Harmonics")
		    (pinch    g2  "Pinch harmonics")
		    (scrapes  gs2 "")
		    (steck8va a2  "Staccato drop octave RR")
		    (palm     as2 "Palm RR")
		    (steck    b2  "Staccato mute RR")
		    (perf     c2  "Performance")
		    (perf2    cs2 "Performance 2")
		    (scrapes  d2  "Scrape effects")
		    (scrapes2 ds2 "Scrape effects")
		    (fx-screem e2  "Effects scream")
		    (fx-single f2  "Effects single notes")
		    (clusters  fs2 "Effects clusters"))))	 
      
      (carvin-neck-rhythm-keyswitch
       (keyswitch 'carvin-neck-rhythm
		  '((down   c2  "Pick down")
		    (up     cs2 "Pick up")
		    (double d2  "Pick double (sustain) RR")
		    (slide  ds2 "Slide up/down (link slide slide2)")
		    (slide2 e2  "Slide up/down (link slide slide2)")
		    (palm   f2  "Palm medium RR")
		    (palm2  fs2 "Palm short RR")
		    (efx    g2  "Rhythmic effects"))))

      (jaguar-keyswitch
       (keyswitch 'jaguar
		  '((sus     c2  "Sustain no vibrato RR")
		    (vib     cs2 "Sustain with vibrato RR")
		    (sus3    d2  "3 second sustain, no vibrato RR")
		    (vib3    ds2 "3 second sustain with vibrato RR")
		    (strum   e2  "Strum long position 1, RR")
		    (strum2  f2  "Strum long position 2, RR")
		    (hammer  fs2 "Hammer on/Pull off  (link hammer slide)")
		    (slide   g2  "Slide up/down       (link hammer slide)")
		    (slide2  gs2 "Slide up with half-tone vibrato")
		    (slide3  a2  "Slide up with whole-tone vibrato")
		    (scrape  as2 "Long scrapes")
		    (scrape2 b2  "Short scrapes")
		    (trem    c3  "Tremolo")
		    (trem2   cs3 "Tremolo mute")
		    (perf    d3  "Performance 120 BPM")
		    (short   ds3 "Strum short RR")
		    (perf2   e3  "Performance 1/4 note")
		    (fall    f3  "Octave fall")
		    (noise   fs3 ""))))
			    

      (jaguar-rhythm-keyswitch
       (keyswitch 'jaguar-rhythm
		  '((long   c2  "Strum long RR, position 1")
		    (long2  cs2 "Strum long RR, position 2")
		    (short  d2  "Strum short"))))

      (lespaul-keyswitch
       (keyswitch 'lespaul
		  '((long    a1  "Long no vibrato RR")
		    (vib     as1 "Medium vibrato RR")
		    (vib2    b1  "Hard vibrato RR")
		    (hammer  c2  "Hammer on / Pull off  (link hammer slide)")
		    (slide   cs2 "Slide up/down         (link hammer slide)")
		    (hammer-vib   d2  "Hammer on/off with hard vibrato (link hammer-vib slide-vib)")
		    (slide-vib    ds2 "Slide up/down with hard slow vibrato (link hammer-vib slide-vib)")
		    (bend         e2  "Bend up")
		    (bend2        f2  "Bend down")
		    (scream-bend  fs2 "Scream, bend up")
		    (scream-bend2 g2  "Scream, fast bend down")
		    (scream-bend3 gs2 "Scream, fast bend up")
		    (scream-vib   a2  "Scream, vibrato, bend up")
		    (pinch        as2 "Pinch harmonics")
		    (stac         b2  "Staccato RR")
		    (mute         c3  "Staccato mute RR")
		    (perf         cs3 "Performance 1/16 notes"))))

      (lespaul-rhythm-keyswitch
       (keyswitch 'lespaul-rhythm
		  '((long    a1  "Long fast RR")
		    (slow    as1 "Long slower RR")
		    (death   b1  "Long death RR")
		    (hammer  c2  "Hammer on/pull off  (link hammer slide)")
		    (slide   cs2 "Slide up/down       (link hammer slide)")
		    (stac    d2  "Staccato RR")
		    (palm    e2  "Palm mute long RR")
		    (palm2   f2  "Palm mute medium RR")
		    (palm3   fs2 "Palm mute short RR")
		    (chug    g2  "Chug long RR")
		    (perf    gs2 "Chug performance fast")
		    (perf2   a2  "Performance")
		    (perf3   as2 "Medium performance")
		    (gallup  b2  "Performance")
		    (chug2   c3  "Chug RR")
		    (chug3   cs3 "Chug double mute")
		    (chug-fx d3  "Chug effects")
		    (chug-harm ds3  "Chug harmonics")
		    (chug-squall e3 "Chug death squall"))))
		    
      (schecter-keyswitch
       (keyswitch 'schecter
		  '((sus    c2  "Sustain no-vibrato")
		    (vib    cs2 "Sustain with vibrato RR")
		    (motto  d2  "Motto vibrato")
		    (hammer ds2 "Hammer on/Pull off  (link hammer slide)")
		    (slide  e2  "Slide up/down       (link hammer slide)")
		    (scrape-vib   f2  "Scrape with vibrato")
		    (screeam      fs2 "Scram with fall vibrato")
		    (pinch  g2  "Pinch harmonic")
		    (harm   gs2 "Harmonics")
		    (scream-fall  a2  "Scream with fall")
		    (mute   as2  "Staccato mute RR"))))

      (schecter-rhythm-keyswitch
       (keyswitch 'schecter-rhythm
		  '((long   c2  "Long sustain RR")
		    (short  cs2 "Short RR")
		    (death  d2  "Long death RR")
		    (death2 ds2 "Long slow death")
		    (long2  e2  "Long slow RR")
		    (mute   f2  "Mute long RR")
		    (mute2  fs2 "Mute medium RR")
		    (mute3  g2  "Mute short RR")
		    (stac   gs2 "Staccato fast RR")
		    (perf   a2  "Performance fast")
		    (hammer as2 "hammer on/pull off (link hammer slide)")
		    (slide  b2  "slide up/down      (link hammer slide)")
		    (perf2  c3  "Performance fast chugs")
		    (chug2  cs3 "Chug with vibrato"))))

      (thinline-keyswitch
       (keyswitch 'thinline
		  '((sus     c2  "Sustain no vibrato RR")
		    (vib     cs2 "Sustain with vibrato RR")
		    (vib2    d2  "Vibrato accent")
		    (pos1    ds2 "Rhythm position 1 RR")
		    (pos2    e2  "Rhythm position 2 RR")
		    (hammer  f2  "Hammer on/Pull off (link hammer slide)")
		    (slide   fs2 "Slide up/down      (link hammer slide)")
		    (scream  g2  "Mild scream")
		    (bend    gs2 "Bend down fast")
		    (fall    a2  "Fall down")
		    (fall2   as2 "Fall up")
		    (harm    b2  "Harmonics (3 key areas)")
		    (stec    c3  "Staccato RR")
		    (scrape  cs3 "")
		    (noise   d3  "Strum noise")
		    (noise2  ds3 "Strum noise 2"))))

      (thinline-rhythm-keyswitch
       (keyswitch 'thinline-rhythm
		  '((pos1   c2  "Position 1 RR")
		    (pos2   cs2 "Position 2 RR")
		    (noise  d2  "Strum noise 1")
		    (noise2 ds2 "Strum noise 2"))))
      
      (remtext (str+ (format nil "Ministry Of Rock II Guitars:~%")
		     (format nil "   Baritone~%")
		     (format nil "   Baritone Rhythm~%")
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
		     (format nil "   Thinline (Telecaster hollow body)~%")
		     (format nil "   Thinline Rhythm (Telecaster hollow body~%"))))

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
