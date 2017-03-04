;;;; cyco local quantumleap mor2-drums
;;;;


(in-package :cyco)

(let ((jazzman-keyswitch
       (keyswitch 'jazzman
		  '((sus      C1  "Sustain"         :range (d1 ds4))
		    (vib      CS1 "Sustain vibrato" :range (d1 ds4))
		    (slide    D1  "Staccato"        :range (d1 ds4))
		    (slide2   DS1 "Slide up slow"   :range (d1 ds4))
		    (slide3   E1  "Slide up medium" :range (d1 cs4))
		    (slide4   G1  "Slide up fast"   :range (d1 d4))
		    (slide5   GS1 "Slide down"      :range (d1 ds4))
		    (slide6   A1  "Slide up octave" :range (d1 ds3))
		    (hammer   AS1 "Hammer"          :range (d1 ds3))
		    (hammer2  B1  "Hammer 2"        :range (d1 a3)))))
      (gibson-eb2-keyswitch
       (keyswitch 'gibson-eb2
		  '((sus      C1  "Open sustain RR"  :range (d1 ds4))
		    (mute     CS1 "Mute RR"          :range (d1 e4))
		    (slide    D1  "Slide up sustain" :range (d1 ds4))
		    (slide2   E1  "Slide up"         :range (d1 ds4))
		    (bend     DS1 "Bend up"          :range (d1 ds4))
		    (trem     F1  "Tremolo"          :range (d1 ds4))
		    (noise    FS1 "Noise"            :range (d1 ds4)))))
      (hofner-keyswitch
       (keyswitch 'hofner
		  '((sus      C1  "Sustain RR"  :range (d1 f4))
		    (slide    CS1 "Slide up"    :range (d1 f4))
		    (noise    D1  "Noise"       :range (d1 b2)))))
      (hofner-fingered-keyswitch
       (keyswitch 'hofner-fingered
		  '((sus      C1  "Sustain"     :range (d1 f4))
		    (slide    CS1 "Slide up"    :range (d1 f4))
		    (trem     D1  "Tremolo"     :range (d1 cs4))
		    (noise    DS1 "Noise"       :range (d1 g3)))))
      (lakland-keyswitch
       (keyswitch 'lakland
		  '((open     A1  "Open down"          :range (b0 f4))
		    (open2    AS1 "Open up & down split range" :range (b0 as2 b3 b6))
		    (legato   B1  "Legato"             :range (b0 f4))
		    (slide    C2  "Slide down"         :range (b0 f4))
		    (slide2   CS2 "Slide down fast"    :range (b0 f4))
		    (slide3   E2  "Slide up sustain"   :range (b0 f4))
		    (brrr     FS2 "Brrr"               :range (b0 d4))
		    (mute     G2  "Mute down"          :range (b0 f4))
		    (mute2    GS2 "Mute up & down split range" :range (b0 as2 b3 b6)))))
      (lakland-fingered-keyswitch
       (keyswitch 'lakland-fingerd
		  '((sus      C1  "Open sustain"       :range (b0 f4))
		    (legato   CS1 "Legato"             :range (b0 f4))
		    (slide    D1  "Slide down"         :range (b0 f4))
		    (slide2   E1  "Slide up sustain"   :range (b0 f4))
		    (brrr     FS1 "Brrr"               :range (b0 d4)))))
      (musicman-keyswitch
       (keyswitch 'musicman
		  '((sus      A0  "Sustain RR"         :range (b0 b4))
		    (open     AS0 "Open down"          :range (b0 f4))
		    (legato   B0  "Legato"             :range (b0 f4))
		    (slide    CS1 "Slide down fast"    :range (b0 f4))
		    (slide2   E1  "Slide up sustain"   :range (b0 f4))
		    (slide3   A1  "Slide down mute"    :range (b0 f4))
		    (brrr     FS1 "Brrr"               :range (b0 ds4))
		    (mute     G1  "Mute down"          :range (b0 f4))
		    (mute2    GS1 "Mute RR"            :range (b0 as4)))))
      (rickenbacker-keyswitch 
       (keyswitch 'rickenbacker
		  '((sus      C1  "Sustain RR"  :range (d1 ds4))
		    (slide    CS1 "Slide up"    :range (d1 ds4))
		    (noise    D1  "Noise"       :range (d1 c3)))))
      (silvertone-keyswitch 
       (keyswitch 'mor2-silvertone
		  '((sus      C1  "Sustain RR"  :range (d1 g4))
		    (legato   CS1 "Legato"      :range (d1 g4))
		    (slide    D1  "Slide up"    :range (d1 g4))
		    (slide2   DS1 "Slide down"  :range (d1 g4))
		    (noise    E1  "Noise"       :range (d1 cs4)))))
      (silvertone-fingered-keyswitch 
       (keyswitch 'mor2-silvertone-fingered
		  '((sus      C1  "Sustain"     :range (d1 d4))
		    (slide    CS1 "Slide up"    :range (d1 d4))
		    (slide2   DS1 "Slide down"  :range (d1 d4))
		    (bend     D1  "Bend up"     :range (d1 d4))
		    (noise    E1  "Noise"       :range (d1 a3)))))
      (stingray-keyswitch 
       (keyswitch 'mor2-stingray
		  '((long     A0  "Long RR"            :range (a0 ds4))
		    (short    AS0 "Short RR"           :range (a0 ds4))
		    (mute     B0  "Mute RR"            :range (a0 ds4))
		    (hammer   C1  "Hammer-n-pull (linked)" :range (a0 ds4))
		    (slide    CS1 "Slide (linked)"     :range (a0 ds4))
		    (fall     D1  "Fall"               :range (b0 ds4))
		    (bend     E1  "Bend up & down"     :range (a0 ds4))
		    (rep      F1  "Fast rep RR"        :range (a0 ds4))
		    (perf     FS1 "Performance fast"   :range (a0 ds4))
		    (hammer2  G1  "Hammer-n-pull short" :range (a0 ds4)))))
      (stingray-fingered-keyswitch 
       (keyswitch 'mor2-stingray-fingered
		  '((long     A0  "Long RR"            :range (a0 ds4))
		    (short    AS0 "Short RR"          :range (a0 ds4))
		    (mute     B0  "Mute RR"            :range (a0 ds4))
		    (hammer   C1  "Hammer-n-pull (linked)" :range (b0 ds4))
		    (slide    CS1 "Slide (linked)"     :range (a0 ds4))
		    (fall     D1  "Fall"               :range (b0 ds4))
		    (bend     E1  "Bend up"            :range (b0 ds4)))))
      (remtext (str+ (format nil "Ministry Of Rook II Basses:~%")
		     (format nil "    Jazzman fretless~%")
		     (format nil "    Gibson EB2~%")
		     (format nil "    Hofner~%")
		     (format nil "    Hofner Fingered~%")
		     (format nil "    Lakland~%")
		     (format nil "    Lakland fingered~%")
		     (format nil "    Musicman~%")
		     (format nil "    Silvertone~%")
		     (format nil "    Silvertone Fingered~%")
		     (format nil "    Stingray 5-string~%")
		     (format nil "    Stingray 5-string Fingered"))))

  (param mor2-bass (create-instrument 'mor2-bass
				      :parent ql1
				      :transient nil
				      :remarks remtext))

	 
  (param jazzman nil)
  (param gibson-eb2 nil)
  (param hofner nil)
  (param hofner-fingered nil)
  (param lakland nil)
  (param lakland-fingered nil)
  (param musicman nil)
  (param rickenbacker nil)
  (param silvertone nil)
  (param silvertone-fingered nil)
  (param stingray nil)
  (param stingray-fingered nil)

  (defun jazzman (&key (parent mor2-bass))
    (setf jazzman (create-instrument 'jazzman
				     :parent parent
				     :transient t
				     :program-change-hook jazzman-keyswitch
				     :remarks "Fretless Fender Jazzman"))
    jazzman)
  
    (defun gibson-eb2 (&key (parent mor2-bass))
      (setf gibson-eb2 (create-instrument 'gibson-eb2
					  :parent parent
					  :transient t
					  :program-change-hook gibson-eb2-keyswitch
					  :remarks ""))
	    
      gibson-eb2)

    (defun hofner (&key (parent mor2-bass))
      (setf hofner (create-instrument 'hofner
				      :parent parent
				      :transient t
				      :program-change-hook hofner-keyswitch
				      :remarks ""))
      hofner)

    (defun hofner-fingered (&key (parent mor2-bass))
      (setf hofner-fingered (create-instrument 'hofner-fingered
					       :parent parent
					       :transient t
					       :program-change-hook hofner-fingered-keyswitch
					       :remarks ""))
      hofner-fingered)

    (defun lakland (&key (parent mor2-bass))
      (setf lakland (create-instrument 'lakland
				       :parent parent
				       :transient t
				       :program-change-hook lakland-keyswitch
				       :remarks ""))
      lakland)

    (defun lakland-fingereed (&key (parent mor2-bass))
      (setf lakland-fingereed (create-instrument 'lakland-fingereed
						 :parent parent
						 :transient t
						 :program-change-hook lakland-fingereed-keyswitch
						 :remarks ""))
      lakland-fingereed)

    (defun musicman (&key (parent mor2-bass))
      (setf musicman (create-instrument 'musicman
					:parent parent
					:transient t
					:program-change-hook musicman-keyswitch
					:remarks ""))
      musicman)

    (defun rickenbacker (&key (parent mor2-bass))
      (setf rickenbacker (create-instrument 'rickenbacker
					    :parent parent
					    :transient t
					    :program-change-hook rickenbacker-keyswitch
					    :remarks ""))
      rickenbacker)

    (defun silvertone (&key (parent mor2-bass))
      (setf silvertone (create-instrument 'silvertone
					  :parent parent
					  :transient t
					  :program-change-hook silvertone-keyswitch
					  :remarks ""))
      silvertone)

    (defun silvertone-fingered (&key (parent mor2-bass))
      (setf silvertone-fingered (create-instrument 'silvertone-fingered
						   :parent parent
						   :transient t
						   :program-change-hook silvertone-fingered-keyswitch
						   :remarks ""))
      silvertone-fingered)

    (defun stingray (&key (parent mor2-bass))
      (setf stingray (create-instrument 'stingray
					:parent parent
					:transient t
					:program-change-hook stingray-keyswitch
					:remarks "5 String Stingray"))
      stingray)

    (defun stingray-fingered (&key (parent mor2-bass))
      (setf stingray-fingered (create-instrument 'stingray-fingered
						 :parent parent
						 :transient t
						 :program-change-hook stingray-fingered-keyswitch
						 :remarks "5 String Stingray"))
      stingray-fingered))
