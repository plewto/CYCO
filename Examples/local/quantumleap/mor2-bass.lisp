;;;; cyco local quantumleap mor2-drums
;;;;


(in-package :cyco)

(let ((jazzman-keyswitch
       (keyswitch 'jazzman
		  '((SUS     C2    "Sustain")
		    (VIB     CS2   "Sustain strong vibrato")
		    (STAC    D2    "Staccato")
		    (SLIDE   DS2   "Slide up Sustain slow")
		    (SLIDE2  E2    "Slide up sustain medium")
		    (SLIDE3  F2    "Slide up slow")
		    (SLIDE4  FS2   "Slide up medium")
		    (SLIDE5  G2    "Slide up fast")
		    (SLIDE-  GS2   "Slide down")
		    (SLIDE8  A2    "Slide up octave")
		    (HARM1   AS2   "Harmonics 1")
		    (HARM2   B2    "Harmonics 2")
		    (FX      C3    "Effects"))))
      
      (gibson-eb2-keyswitch
       (keyswitch 'gibson-eb2
		  '((SUS     C2    "Sustain open RR")
		    (MUTE    CS2   "Mute RR")
		    (SLIDE   D2    "Slide up sustain")
		    (BEND    DS2   "Bend up sustain")
		    (SLIDE2  E2    "Slide up")
		    (TREM    F2    "Tremolo")
		    (NOISE   FS2   "Noises")
		    (NOISE2  G2    "Noises"))))
      
      (hofner-keyswitch
       (keyswitch 'hofner
		  '((SUS    C2  "Sustain")
		    (SLIDE  CS2 "Slide up")
		    (TREM   D2  "Tremolo")
		    (NOISE  DS2 "Noise"))))
      
      (hofner-fingered-keyswitch
       (keyswitch 'hofner-fingerd
		  '((SUS    C2  "Sustain")
		    (SLIDE  CS2 "Slide up sustain")
		    (NOISE  D2  "Noise"))))

      (lakland-keyswitch
       (keyswitch 'lakland
		  '((OPEN     A1  "Open down")
      		    (OPEN2    AS1 "Open up/down (dual range)")
      		    (LEG      B1  "Legatto")
      		    (SLIDE    C2  "Open slide down")
      		    (SLIDE2   CS2 "Open slide down fast")
		    (SLIDE3   D2  "Open slide up")
		    (SLIDE4   DS2 "Open slide up fast")
		    (SLIDE5   E2  "Open slide up sustain")
		    (OCTAVE   F2  "Octave")
		    (BRRR     FS2 "Burr")
		    (MUTE     G2  "Mute down")
		    (MUTE2    GS2 "Mute Up/Down (dual range)")
		    (MUTE3    A2  "Mute slide down")
		    (MUTE4    AS2 "Mute slide up"))))

      (lakland-fingered-keyswitch
       (keyswitch 'laklad
		  '((OPEN    C2  "Open sustain")
		    (LEG     CS2 "Legatto")
		    (SLIDE   D2  "Slide down")
		    (SLIDE2  DS2 "Slide up")
		    (SLIDE3  E2  "Slide up sustain")
		    (OCTAVE  F2  "Octave")
		    (BRRR    FS2 "Burr"))))
      
      (musicman-keyswitch
       (keyswitch 'musicman
		  '((SUS    A1  "Sustain RR")
		    (OPEN   AS1 "Open down")
		    (LEG    B1  "Leggato")
		    (SLIDE  C2  "Open slide down")
		    (SLIDE2 CS2 "Open slide down fast")
		    (SLIDE3 D2  "Open slide up")
		    (SLIDE4 DS2 "Open slide up fast")
		    (SLIDE5 E2  "Open slide up sustain")
		    (OCTAVE F2  "Octave")
		    (BRRR   FS2 "BRRR")
		    (MUTE   G2  "Mute down")
		    (MUTERR GS2 "Mute RR")
		    (MUTE2  A2  "Mute slide down")
		    (MUTE3  AS2 "Mute slide up"))))

      (rickenbacker-keyswitch
       (keyswitch 'rickenbacker
		  '((SUS    C2  "Sustain RR")
		    (SLIDE  CS2 "Slide up sus")
		    (NOISE  D2  "Noise")
		    (NOISE2 DS2 "Noise"))))

      (silvertone-keyswitch
       (keyswitch 'silvertone
		  '((SUS    C2  "Sustain RR")
		    (LEG    CS2 "Leggato")
		    (SLIDE  D2  "Slide up sustain")
		    (SLIDE2 DS2 "Slide down")
		    (NOISE  E0  "Noise"))))

      (silvertone-fingered-keyswitch
       (keyswitch 'silvertone-fingered
		  '((SUS    C2  "Sustained")
		    (SLIDE  CS2 "Slide up")
		    (BEND   D2  "Bend up")
		    (SLIDE2 DS2 "Slide down")
		    (NOISE  E2  "Noise"))))

      (stingray-keyswitch
       (keyswitch 'stingray
		  '((LONG   A1  "Sustain long RR")
		    (SHORT  AS1 "Sustain short RR")
		    (MUTE   B1  "Sustain mute RR")
		    (SUS+HAMMER  C2 "Sustain (link slide)")
		    (SLIDE       CS2 "Slide up & Down (link sus+hammer)")
		    (FALL   D2  "")
		    (DOIT   DS2 "")
		    (BEND   E2  "Bend up/down")
		    (REP    F2  "Fast rep RR")
		    (REP2   FS2 "Fast rep perf")
		    (HAMMER G2  "Hammer on/off (link sus+hammer)")
		    (NOISE  GS2 ""))))
      
      (stingray-fingered-keyswitch
       (keyswitch 'stingray-fingered
		  '((LONG    A1  "Sustain long RR")
		    (SHORT   AS1 "Sustain short RR")
		    (MUTE    B1  "Sustain mute RR")
		    (HAMMER  C2  "Hammer on/pull off (link slide)")
		    (SLIDE   CS2 "Slide up/down (link hammer)")
		    (FALL    D2  "")
		    (DOIT    DS2 "")
		    (BEND    E2  "Bend up/down")
		    (NOISE   F2 ""))))
      
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
