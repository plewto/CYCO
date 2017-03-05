;;;; quatumleap vop
;;;;
;;;;  gypsy               -> QL3
;;;;  gypsy-guitar        -> QL4 (same as mor2-guitar) 
;;;;

(in-package :cyco)

(let ((cimbalon-keyswitch
       (keyswitch 'cimbalon
		  '((sus     C2  "Sustain")
		    (double  CS2 "Double")
		    (tremolo D2  "Tremolo"))))
       (trombone-keyswitch
	(keyswitch 'trombone
		   '((sus           C2  "Sustain"        )
		     (sus-farty     CS2 "Sustain-Farty"  )
		     (staccato      D2  "Staccato"       )
		     (staccato-long DS2 "Staccato-long"  )
		     (marc          E2  "Marc"           )
		     (dimuendo      F2  "Dimuendo"       )
		     (sforzando     FS2 "Sforzando"      )
		     (crescendo     G2  "Crescendo"      ))))
       (violin-keyswitch
	(keyswitch 'violin
		   '((sus-vib         C2  "sus-vib"         )
		     (sus-vib2        CS2 "sus-vib2"        )
		     (sus-vib3        D2  "sus-vib3"        )
		     (passionato      DS2 "passionato"      )
		     (expressive      E2  "expressive"      )
		     (expressive-slow F2  "expressive-slow" )
		     (sus-vib-accent  FS2 "sus-vib-accent"  )
		     (sforzando       G2  "sforzando"       )
		     (sul-tasto       GS2 "sul-tasto"       )
		     (sul-tasto-exp   A2  "sul-tasto-exp"   )
		     (accent-novib    AS2 "accent-novib"    )
		     (sforzando-novib B2  "aforzando-novib" )
		     (sus-novib       C3  "sus-novib"       )
		     (martele1        CS3 "martele1"        )
		     (martele2        D3  "martele2"        )
		     (spiccato        DS3 "spiccato"        )
		     (spiccato2       E3  "spiccato2"       )
		     (spiccato-long   F3  "spiccato-long"   )
		     (spiccato-long2  FS3 "spiccato-long2"  )
		     (left-pizzacato  G3  "left-pizzacato"  )
		     (repitions       GS3 "repititions"     )
		     (harmonics       A3  "harmonics"       )
		     (ponticello      AS3 "ponticello"      )
		     (bounce          B3  "bounce"          ))))
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
		  '((sus          C2  "Sus"       )
		    (sforzando    CS2 "Sforzando" )
		    (portato      D2  "Portato"   )
		    (short        DS2 "Short"     )
		    (crescendo    E2  "Crescendo" )
		    (sus-accent-1 F2  "sus-accent-1" )
		    (sus-accent-2 F2  "sus-accent-2" ))))
      (campana-keyswitch
       (keyswitch 'campana
		  '((out-full    C2  "out-left-full"   )
		    (in-octave   CS2 "in-left-octave"  )
		    (out-single  D2  "out-left-single" )
		    (in-single   DS2 "in-left-single"  )
		    (out-major   E2  "out-left-major"  )
		    (out-minor   F2  "out-left-minor"  )
		    (out-7th     FS2 "out-left-7th"    ))))
       (excelsior-keyswitch
	(keyswitch 'excelsior
		   '((out-full    C2  "out-left-full"   )
		     (in-octave   CS2 "in-left-octave"  )
		     (out-single  D2  "out-left-single" )
		     (in-single   DS2 "in-left-single"  )
		     (out-major   E2  "out-left-major"  )
		     (out-minor   F2  "out-left-minor"  )
		     (out-7th     FS2 "out-left-7th"    ))))
       (silvestri-keyswitch
	(keyswitch 'silvestri
		   '((out-full    C2  "out-left-full"  )
		     (in-octave   CS2 "in-left-octave" )
		     (out-major   D2  "out-left-major" )
		     (out-minor   DS2 "out-left-minor" )
		     (out-7       E2  "out-left-7"     ))))
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
		  '((sus        C2  "sus"        :range (E1 A4))
		    (sus-vib    CS2 "sus-vib"    :range (E1 A4))
		    (legato     D2  "legato"     :range (E1 A4))
		    (harmonics  DS2 "harmonics"  :range (E1 e5))
		    (mute-strum E2  "mute-strum" :range (E1 A4))
		    (fret-noise F2  "fret-noise" :range (E1 C6)))))
      (django-keyswitch
       (keyswitch 'gypsy-django
		  '((short (C2 "short" :range (E1 D5)))
		    (long  (CS2 "long" :range (E1 D5))))))
      (django-chords-keyswitch
       (keyswitch 'gypsy-django-chords
		  '((maj           C2  "major"     :range (E1 D5))
		    (min           CS2 "minor"     :range (E1 D5))
		    (dom7          D2  "dom7"      :range (E1 D5))
		    (maj9          DS2 "maj9"      :range (E1 D5))
		    (dim7          E2  "dim7"      :range (E1 D5))
		    (maj6          F2  "maj6"      :range (E1 D5))
		    (min7          FS2 "min7"      :range (E1 D5))
		    (maj7          G2  "maj7"      :range (E1 D5))
		    (seven-flat-5  GS2 "seven-flat-5" :range (E1 D5)))))
      (flamenco-keyswitch
       (keyswitch 'gypsy-flamenco
		  '((sus         C2  "Sustain"        :range (E1 A4))
		    (sus-bridge  CS3 "Sustain-bridge" :range (E1 A4))
		    (trem        D3  "Tremolo"        :range (E1 D4))
		    (taps        DS3 "Taps"           :range (D2 C6)))))
      (flamenco-chords-keyswitch
       (keyswitch 'gypsy-flemenco-chords
		  '((fast-major    C2  "fast-major")
		    (fast-minor    CS2 "fast-minor")
		    (arpeg-major   D2  "arpeg-major")
		    (arpeg-minor   DS2 "arpeg-minor")
		    (trem-major    E2  "trem-major")
		    (trem-minor    F2  "trem-minor")
		    (roll-major    FS2 "roll-major")
		    (roll-minor    G2  "roll-minor")
		    (double-major  GS2 "double-major")
		    (double-minor  A2  "double-minor")
		    (sus-major     AS2 "sus-major")
		    (sus-minor     B2  "sus-minor")
		    (sus-vs-maj    C3  "sus-vs-maj")
		    (sus-vs-min    CS3 "sus-vs-min")
		    (stacato-maj   D3  "stacato-maj")
		    (stacato-min   DS3 "stacato-min"))))
      (spanish-keyswitch
       (keyswitch 'gypsy-spanish
		  '((sus               C2  "sus")
		    (strum             CS2 "strum")
		    (strum-fast        D2  "srtum-fast")
		    (half-slide        DS2 "half-slide")
		    (whole-slide       E2  "whole-slide")
		    (half-slide-up     F2  "half-slide-up")
		    (half-slide-down   FS2 "half-slide-down")
		    (staccato-mute-pop G2  "stccato-mute-pop")
		    (staccato          GS2 "staccato")
		    (harmonics         A2  "harmonics")
		    (legato-sus        AS2 "legato-sus")
		    (noises            B2  "noises")
		    (neck-slides       C3  "neck-slides, range in 4 discreet areas.")
		    (pitchless-chugs   CS3 "pitchless-chugs")
		    (taps              D3  "taps")))))

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
