;;;; cyco local quantumleap gypsy accordions
;;;;

(in-package :cyco)

(param gypsy1 (create-instrument
	       'gypsy1
	       :parent quantumleap
	       :transient nil
	       :channel :gypsy1))

;;; ---------------------------------------------------------------------- 
;;;				 Bandoneon

(param bandoneon nil)

(defun bandoneon (&key (parent gypsy1))
  (free-orchestra! :node parent)
  (setf bandoneon
	(create-instrument
	 'bandoneon
	 :parent parent
	 :transient t
	 :program-change-hook
	 (keyswitch 'bandoneon
		    '((sus          . (C2  "Sus"       :range (c1 b6)))
		      (sforzando    . (CS2 "Sforzando" :range (c1 b6)))
		      (portato      . (D2  "Portato"   :range (c1 b6)))
		      (short        . (DS2 "Short"     :range (c1 b6)))
		      (crescendo    . (E2  "Crescendo" :range (c1 b6)))
		      (sus-accent-1 . (F2  "sus-accent-1" :range (c1 b6)))
		      (sus-accent-2 . (F2  "sus-accent-2" :range (c1 b6))))))))

;;; ---------------------------------------------------------------------- 
;;;				 Campana
;;;
;;; Campana double-reed, single-reed and musette have identical structure.

(param campana nil)

(defun campana (&key (parent gypsy1))
  (free-orchestra! :node parent)
  (create-instrument
   'campana
   :parent parent
   :transient t
   :program-change-hook
   (keyswitch
    'gypsy-campana
    '((out-left-full    . (C2  "out-left-full"   :range (c0 a5)))
      (in-left-octave   . (CS2 "in-left-octave"  :range (c0 a5)))
      (out-left-single  . (D2  "out-left-single" :range (c0 a5)))
      (in-left-single   . (DS2 "in-left-single"  :range (c0 a5)))
      (out-left-major   . (E2  "out-left-major"  :range (c0 a5)))
      (out-left-minor   . (F2  "out-left-minor"  :range (c0 a5)))
      (out-left-7th     . (FS2 "out-left-7th"    :range (c0 a5)))))))

;;; ---------------------------------------------------------------------- 
;;;				 Excelsior
;;;
;;; keyswitch applies only to left hand chord section.
;;; Range left  (c0  b1)
;;; Range right (f1  a5)
;;;
;;; Excelsior double-reed, single-reed and musette have identical structure.

(param excelsior nil)

(defun excelsior (&key (parent gypsy1))
  (free-orchestra! :node parent)
  (setf excelsior
	(create-instrument
	 'gypsy-excelsior
	 :parent parent
	 :transient t
	 :program-change-hook
	 (keyswitch
	  'excelsior
	  '((out-left-full    . (C2  "out-left-full"   :range (c0 a5)))
	    (in-left-octave   . (CS2 "in-left-octave"  :range (c0 a5)))
	    (out-left-single  . (D2  "out-left-single" :range (c0 a5)))
	    (in-left-single   . (DS2 "in-left-single"  :range (c0 a5)))
	    (out-left-major   . (E2  "out-left-major"  :range (c0 a5)))
	    (out-left-minor   . (F2  "out-left-minor"  :range (c0 a5)))
	    (out-left-7th     . (FS2 "out-left-7th"    :range (c0 a5))))))))

;;; ---------------------------------------------------------------------- 
;;;				 Silvestri
;;;
;;; Keyswitch applies only to left hand chord section.
;;; Range left  (c0  b1)
;;; Range right (f1  a5)
;;;
;;; Silvestri single-reed and musette have identical structure.

(param silvestri nil)

(defun silvestri (&key (parent gypsy1))
  (free-orchestra! :node parent)
  (setf silvestri
	(create-instrument
	 'gypsy-silvestri
	 :parent parent
	 :transient t
	 :program-change-hook
	 (keyswitch
	  'silvestri
	  '((out-left-full    . (C2  "out-left-full"  :range (c0 a5)))
	    (in-left-octave   . (CS2 "in-left-octave" :range (c0 a5)))
	    (out-left-major   . (D2  "out-left-major" :range (c0 a5)))
	    (out-left-minor   . (ds2 "out-left-minor" :range (c0 a5)))
	    (out-left-7       . (e2  "out-left-7"     :range (c0 a5))))))))
