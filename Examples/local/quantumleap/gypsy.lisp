;;;; quatumleap vop
;;;;
;;;;  gypsy               -> QL3
;;;;  gypsy-guitar        -> QL4 (same as mor2-guitar) 
;;;;

(in-package :cyco)

(let ((cimbalon-keyswitch
       (keyswitch 'cimbalon
		  '((sus     C1  "Sustain")
		    (double  CS1 "Double")
		    (tremolo D1  "Tremolo"))))
       (trombone-keyswitch
	(keyswitch 'trombone
		   '((sus           C1  "Sustain"        )
		     (sus-farty     CS1 "Sustain-Farty"  )
		     (staccato      D1  "Staccato"       )
		     (staccato-long DS1 "Staccato-long"  )
		     (marc          E1  "Marc"           )
		     (dimuendo      F1  "Dimuendo"       )
		     (sforzando     FS1 "Sforzando"      )
		     (crescendo     G1  "Crescendo"      ))))
       (violin-keyswitch
	(keyswitch 'violin
		   '((sus-vib         C1  "sus-vib"         )
		     (sus-vib2        CS1 "sus-vib2"        )
		     (sus-vib3        D1  "sus-vib3"        )
		     (passionato      DS1 "passionato"      )
		     (expressive      E1  "expressive"      )
		     (expressive-slow F1  "expressive-slow" )
		     (sus-vib-accent  FS1 "sus-vib-accent"  )
		     (sforzando       G1  "sforzando"       )
		     (sul-tasto       GS1 "sul-tasto"       )
		     (sul-tasto-exp   A1  "sul-tasto-exp"   )
		     (accent-novib    AS1 "accent-novib"    )
		     (sforzando-novib B1  "aforzando-novib" )
		     (sus-novib       C2  "sus-novib"       )
		     (martele1        CS2 "martele1"        )
		     (martele2        D2  "martele2"        )
		     (spiccato        DS2 "spiccato"        )
		     (spiccato2       E2  "spiccato2"       )
		     (spiccato-long   F2  "spiccato-long"   )
		     (spiccato-long2  FS2 "spiccato-long2"  )
		     (left-pizzacato  G2  "left-pizzacato"  )
		     (repitions       GS2 "repititions"     )
		     (harmonics       A2  "harmonics"       )
		     (ponticello      AS2 "ponticello"      )
		     (bounce          B2  "bounce"          ))))
       (remtext (str+ (format nil "Gypsy~%")
		      (format nil "    gypsy-castanets~%")
		      (format nil "    gypsy-dancer~%")
		      (format nil "    gypsy-percussion~%")
		      (format nil "    gypsy-accordian~%")
		      (format nil "        bandoneon~%")
		      (format nil "        campana~%")
		      (format nil "        excelsior~%")
		      (format nil "        silvestri~%")
		      (format nil "    gypsy-guitar~%")
		      (format nil "        classical~%")
		      (format nil "        django~%")
		      (format nil "        django-chords~%")
		      (format nil "        flamenco~%")
		      (format nil "        flamenco-chords~%")
		      (format nil "        spanish~%")
		      (format nil "    cimbalon~%")
		      (format nil "    trombone~%")
		      (format nil "    violin~%"))))
      
      (param gypsy (create-instrument 'gypsy
				      :parent ql3
				      :transient nil
				      :remarks remtext))
      (param gypsy-guitar (create-instrument 'gypsy-guitar
					     :parent ql2
					     :transient nil
					     :remarks remtext))
      (defun gypsy-castanets (&key (parent gypsy))
	(param gypsy-castanets (create-instrument
				'gypsy-castanets
				:parent parent
				:transient t
				:remarks ""))
	gypsy-castanets)

      (defun gypsy-dancer (&key (parent gypsy))
	(param gypsy-dancer (create-instrument
			     'gypsy-dancer
			     :parent parent
			     :transient t
			     :remarks ""))
	gypsy-dancer)

      (defun gypsy-percussion (&key (parent gypsy))
	(param gypsy-percussion (create-instrument
				 'gypsy-percussion
				 :parent parent
				 :transient t
				 :remarks ""))
	gypsy-percussion)

      
      (defun cimbalon (&key (parent gypsy))
	(param cimbalon (create-instrument
			 'cimbalon
			 :parent parent
			 :transient t
			 :program-change-hook cimbalon-keyswitch
			 :remarks ""))
	cimbalon)

      (defun trombone (&key (parent gypsy))
	(param trombone (create-instrument
			 'trombone
			 :parent parent
			 :transient t
			 :program-change-hook trombone-keyswitch
			 :remarks ""))
	trombone)

      (defun violin (&key (parent gypsy))
	(param violin (create-instrument
		       'violin
		       :parent parent
		       :transient t
		       :program-change-hook violin-keyswitch
		       :remarks ""))
	violin)) 

;;; ---------------------------------------------------------------------- 
;;;			      Gypsy Accordians
;;;
;;; gypsy-accordian
;;;    bandoneon
;;;    campana
;;;    excelsior
;;;    silvestri
;;;

(let ((bandoneon-keyswitch
       (keyswitch 'bandoneon
		  '((sus          C1  "Sus"       )
		    (sforzando    CS1 "Sforzando" )
		    (portato      D1  "Portato"   )
		    (short        DS1 "Short"     )
		    (crescendo    E1  "Crescendo" )
		    (sus-accent-1 F1  "sus-accent-1" )
		    (sus-accent-2 F1  "sus-accent-2" ))))
      (campana-keyswitch
       (keyswitch 'campana
		  '((out-full    C1  "out-left-full"   )
		    (in-octave   CS1 "in-left-octave"  )
		    (out-single  D1  "out-left-single" )
		    (in-single   DS1 "in-left-single"  )
		    (out-major   E1  "out-left-major"  )
		    (out-minor   F1  "out-left-minor"  )
		    (out-7th     FS1 "out-left-7th"    ))))
       (excelsior-keyswitch
	(keyswitch 'excelsior
		   '((out-full    C1  "out-left-full"   )
		     (in-octave   CS1 "in-left-octave"  )
		     (out-single  D1  "out-left-single" )
		     (in-single   DS1 "in-left-single"  )
		     (out-major   E1  "out-left-major"  )
		     (out-minor   F1  "out-left-minor"  )
		     (out-7th     FS1 "out-left-7th"    ))))
       (silvestri-keyswitch
	(keyswitch 'silvestri
		   '((out-full    C1  "out-left-full"  )
		     (in-octave   CS1 "in-left-octave" )
		     (out-major   D1  "out-left-major" )
		     (out-minor   DS1 "out-left-minor" )
		     (out-7       E1  "out-left-7"     ))))
       (remtext (str+ (format nil "Gypsy Accordians:~%")
		      (format nil "    Bandoneon~%")
		      (format nil "    Campana~%")
		      (format nil "    Excelsior~%")
		      (format nil "    Silvestri~%"))))

  (param gypsy-accordian (create-instrument 'gypsy-accordian
					    :parent gypsy
					    :transient nil))
  
  (defun bandoneon (&key (parent gypsy-accordian))
    (param bandoneon (create-instrument 
		      'bandoneon
		      :parent parent
		      :transient t
		      :program-change-hook bandoneon-keyswitch
		      :remarks ""))
    bandoneon)

  (defun campana (&key (parent gypsy-accordian))
    (param campana (create-instrument 
		    'campana
		    :parent parent
		    :transient t
		    :program-change-hook campana-keyswitch
		    :remarks "Double, single and musette have identical structure."))
    campana)

  (defun excelsior (&key (parent gypsy-accordian))
    (param excelsior (create-instrument 
		      'excelsior
		      :parent parent
		      :transient t
		      :program-change-hook excelsior-keyswitch
		      :remarks "Double, single and musette have identical structure."))
    excelsior)

  (defun silvestri (&key (parent gypsy-accordian))
    (param silvestri (create-instrument 
		      'silvestri
		      :parent parent
		      :transient t
		      :program-change-hook silvestri-keyswitch
		      :remarks "Single and musette have identical structure."))
    silvestri))


;;; ---------------------------------------------------------------------- 
;;;			       Gypsy Guitar
;;;

(let ((classical-keyswitch
       (keyswitch 'gypsy-classical
		  '((sus        C1  "sus"        :range (E1 A4))
		    (sus-vib    CS1 "sus-vib"    :range (E1 A4))
		    (legato     D1  "legato"     :range (E1 A4))
		    (harmonics  DS1 "harmonics"  :range (E1 e5))
		    (mute-strum E1  "mute-strum" :range (E1 A4))
		    (fret-noise F1  "fret-noise" :range (E1 C6)))))
      (django-keyswitch
       (keyswitch 'gypsy-django
		  '((short (C1 "short" :range (E1 D5)))
		    (long  (CS1 "long" :range (E1 D5))))))
      (django-chords-keyswitch
       (keyswitch 'gypsy-django-chords
		  '((maj           C1  "major"     :range (E1 D5))
		    (min           CS1 "minor"     :range (E1 D5))
		    (dom7          D1  "dom7"      :range (E1 D5))
		    (maj9          DS1 "maj9"      :range (E1 D5))
		    (dim7          E1  "dim7"      :range (E1 D5))
		    (maj6          F1  "maj6"      :range (E1 D5))
		    (min7          FS1 "min7"      :range (E1 D5))
		    (maj7          G1  "maj7"      :range (E1 D5))
		    (seven-flat-5  GS1 "seven-flat-5" :range (E1 D5)))))
      (flamenco-keyswitch
       (keyswitch 'gypsy-flamenco
		  '((sus         C1  "Sustain"        :range (E1 A4))
		    (sus-bridge  CS2 "Sustain-bridge" :range (E1 A4))
		    (trem        D2  "Tremolo"        :range (E1 D4))
		    (taps        DS2 "Taps"           :range (D2 C6)))))
      (flamenco-chords-keyswitch
       (keyswitch 'gypsy-flemenco-chords
		  '((fast-major    C1  "fast-major")
		    (fast-minor    CS1 "fast-minor")
		    (arpeg-major   D1  "arpeg-major")
		    (arpeg-minor   DS1 "arpeg-minor")
		    (trem-major    E1  "trem-major")
		    (trem-minor    F1  "trem-minor")
		    (roll-major    FS1 "roll-major")
		    (roll-minor    G1  "roll-minor")
		    (double-major  GS1 "double-major")
		    (double-minor  A1  "double-minor")
		    (sus-major     AS1 "sus-major")
		    (sus-minor     B1  "sus-minor")
		    (sus-vs-maj    C2  "sus-vs-maj")
		    (sus-vs-min    CS2 "sus-vs-min")
		    (stacato-maj   D2  "stacato-maj")
		    (stacato-min   DS2 "stacato-min"))))
      (spanish-keyswitch
       (keyswitch 'gypsy-spanish
		  '((sus               C1  "sus")
		    (strum             CS1 "strum")
		    (strum-fast        D1  "srtum-fast")
		    (half-slide        DS1 "half-slide")
		    (whole-slide       E1  "whole-slide")
		    (half-slide-up     F1  "half-slide-up")
		    (half-slide-down   FS1 "half-slide-down")
		    (staccato-mute-pop G1  "stccato-mute-pop")
		    (staccato          GS1 "staccato")
		    (harmonics         A1  "harmonics")
		    (legato-sus        AS1 "legato-sus")
		    (noises            B1  "noises")
		    (neck-slides       C2  "neck-slides, range in 4 discreet areas.")
		    (pitchless-chugs   CS2 "pitchless-chugs")
		    (taps              D2  "taps")))))

  (defun classical-guitar (&key (parent gypsy-guitar))
    (param classical-guitar (create-instrument
			     'classical-guitar
			     :parent parent
			     :transient t
			     :remarks ""
			     :program-change-hook classical-keyswitch))
    classical-guitar)

  (defun django-guitar (&key (parent gypsy-guitar))
    (param django-guitar (create-instrument
			  'django-guitar
			  :parent parent
			  :transient t
			  :remarks ""
			  :program-change-hook django-keyswitch))
    django-guitar)

  (defun django-chords-guitar (&key (parent gypsy-guitar))
    (param django-chords-guitar (create-instrument
				 'django-chords-guitar
				 :parent parent
				 :transient t
				 :remarks ""
				 :program-change-hook django-chords-keyswitch))
    django-chords-guitar)

  (defun flamenco-guitar (&key (parent gypsy-guitar))
    (param flamenco-guitar (create-instrument
			    'flamenco-guitar
			    :parent parent
			    :transient t
			    :remarks ""
			    :program-change-hook flamenco-keyswitch))
    flamenco-guitar)

  (defun flamenco-chords-guitar (&key (parent gypsy-guitar))
    (param flamenco-chords-guitar (create-instrument
				   'flamenco-chords-guitar
				   :parent parent
				   :transient t
				   :remarks ""
				   :program-change-hook flamenco-chords-keyswitch))
    flamenco-chords-guitar)

  
  (defun spanish-guitar (&key (parent gypsy-guitar))
    (param spanish-guitar (create-instrument
			   'spanish-guitar
			   :parent parent
			   :transient t
			   :remarks ""
			   :program-change-hook spanish-keyswitch))
    spanish-guitar))
