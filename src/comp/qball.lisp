;;;; cyco.comp.qball
;;;;
;;;; QBALL extends PART and generate MIDI note events by evaluating
;;;; PATTERENs.
;;;;

(in-package :cyco)

(defclass qball (part)
  ((cue-fn
    :type function
    :accessor cue-fn
    :initform #'bar
    :initarg :cue-fn)
   (cue-pattern
    :type cycle
    :accessor cue-pattern
    :initform (cycle :of nil)
    :initarg :cue)
   (key-pattern
    :type pattern
    :accessor key-pattern
    :initform (cycle :of '(60))
    :initarg :key)
   (dur-pattern
    :type pattern
    :accessor dur-pattern
    :initform (cycle :of '(q))
    :initarg :dur)
   (amp-pattern
    :type pattern
    :accessor amp-pattern
    :initform (cycle :of '(ff))
    :initarg :amp)))

(defmacro qball (name instruments &key
		      (cue '())
		      (period nil)
		      (qfn #'bar)
		      (transposable t)
		      (key (cycle :of '(60)))
		      (dur (cycle :of '(q)))
		      (amp (cycle :of '(ff)))
		      (key-map #'identity)
		      (dur-map #'identity)
		      (amp-map #'identity)
		      (project *project*)
		      (section nil))
  "TODO: add docs for transposable
Binds new instance of QBALL to name.
   A QBALL is a PART which uses patterns to generate MIDI note events.
   A QBALL has 4 patterns: cue, key, duration and amplitude.  The cue 
   pattern is always a CYCLE of event times.  For each element in the 
   the cue PATTERN the key, duration and amplitude patterns are evaluated.
   By default the key, duration and amplitude patterns are CYCLEs but 
   may be any PATTERN type.  If the lengths of the various patterns are
   different, complex combinations of the parameters are produced.

   name        - Symbol, the QBALL is bound to name.
   instruments - List of instruments or instrument names.  A single instrument
                 or name may also be specified.
   :cue        - List of time specifications.  The format is a nested list
                 ((time-1)(time-2)...(time-n))  where (time-i) must be a form
                 expected by the qfn argument.  The default qfn is #'BAR so 
                 time expressions should be (bar beat subbeat tick).
   :period     - Duration of this PART in seconds.  The default is to inherit
                 the duration of the parent SECTION.  If period is less then 
                 the SECTION duration, then the QBALL will be repeated as 
                 needed.
   :qfn        - Function used to evaluate time expressions, default #'BAR.
   :key        - List or PATTERN of keynumbers.  If :key is a list it is 
                 converted to a CYCLE. Default '(60)
   :dur        - List or PATTERN of duration values.  If :dur is a list
                 it is converted to a CYCLE. Default '(Q)
   :amp        - List or PATTERN of amplitude values.  If :amp is a list
                 it is converted to a CYCLE. Default '(FF)
   :key-map    - Mapping function applied to key numbers.  This function 
                 is applied to each key-number prior to sending them to 
                 the instruments.  The function has the form 
                 (lambda (k)) -> k'  Any unrecognized values should be 
                 returned as is without producing an error. 
                 Default #'IDENTITY 
   :dur-map    - Mapping function applied to note duration.  This function
                 is applied to each duration prior to sending them to the 
                 instruments.  The function form is (lambda (d)) -> d'
                 Any unrecognized values should be returned as is without
                 producing an error.
   :amp-map    - Mapping function applied to note amplitude.  This function
                 is applied to each amplitude value prior to sending them 
                 to the instruments.   The function form is (lambda (a)) -> a'
                 Any unrecognized values should be returned as is without
                 producing an error.
   :project    - Default *PROJECT*
   :section    - Default current section of project."
  `(progn
     (validate-part-parents ,project ,section)
     (flet ((->pattern (arg)
     		       (cond ((pattern-p arg) arg)
     			     (t (cycle :of (->list arg))))))
       (let* ((sec (or ,section (current-section ,project)))
	      (ilist (->instrument-list ,instruments ,project))
     	      (prt (make-instance 'qball
     				  :name ',name
     				  :instruments ilist
     				  :period (or ,period (duration sec))
				  :cue-fn ,qfn
     				  :cue (cycle :of ,cue)
     				  :key (->pattern ,key)
     				  :dur (->pattern ,dur)
     				  :amp (->pattern ,amp)))) ;
     	 (add-child! sec prt)
     	 ;(property! prt :qfn ,qfn)
     	 (property! prt :keynumber-map ,key-map)
     	 (property! prt :duration-map ,dur-map)
     	 (property! prt :amplitude-map ,amp-map)
	 (property! prt :transposable ,transposable)
     	 (param ,name prt)
	 (part-banner sec prt)
     	 prt))))

(defmethod reset ((obj qball))
  (reset (cue-pattern obj))
  (reset (key-pattern obj))
  (reset (dur-pattern obj))
  (reset (amp-pattern obj))
  obj)

(defmethod dump ((prt qball) &key (depth 0)(max-depth 10))
  (if (< depth max-depth)
      (if (mute? prt)
	  (format t "~AMUTED ~A ~A~%"  (tab depth)(type-of prt)(name prt))
	(let ((pad (tab (1+ depth))))
	  (format t "~A~A ~A~%" (tab depth) (type-of prt)(name prt))
	  (if (< (1+ depth) max-depth)
	      (progn
		(format t "~ACUE: ~A~%" pad (->string (cue-pattern prt)))
		(format t "~AKEY: ~A~%" pad (->string (key-pattern prt)))
		(format t "~ADUR: ~A~%" pad (->string (dur-pattern prt)))
		(format t "~AAMP: ~A~%" pad (->string (amp-pattern prt))))))))
  nil)

(defmethod render-once ((prt qball) &key (offset 0))
  (reset prt)
  (if (not (mute? prt))
      (let ((acc '())
	    (qfn (cue-fn prt))
	    (kmap (property prt :keynumber-map))
	    (dmap (property prt :duration-map))
	    (amap (property prt :amplitude-map))
	    (dscale (beat-duration (parent prt)))
	    (cuepat (cue-pattern prt)))
	(dotimes (i (cardinality cuepat))
	  (let* ((time1 (+ offset (apply qfn (next-1 cuepat))))
		 (k (funcall kmap (next-1 (key-pattern prt))))
		 (d (funcall dmap (next-1 (dur-pattern prt))))
		 (a (funcall amap (next-1 (amp-pattern prt)))))
	    (dolist (inst (instruments prt))
	      (let* ((cindex (1- (channel inst :resolve t)))
		     (k2 (instrument-keynumber inst k))
		     (d2 (* dscale (instrument-duration inst d)))
		     (a2 (instrument-amplitude inst a))
		     (velocity (truncate (* 127 a2)))
		     (time2 (+ time1 d2)))
	      	(setf acc
		      (append acc
			      (list
			       (cons time1 (midi-note-on cindex k2 velocity))
			       (cons time2 (midi-note-off cindex k2 0)))))))))
	acc)
    nil))

(defmethod clone ((p qball) &key
		  (newname "~A-clone")
		  (parent nil)
		  (hook #'identity))
  (let* ((new-name (format nil newname (name p)))
	 (new-parent (or parent (parent p)))
	 (other (make-instance 'qball
			       :parent new-parent
			       :name new-name
			       :instruments (instruments p)
			       :period (period p)
			       :mute (mute? p)
			       :cue-fn (cue-fn p)
			       :cue (clone (cue-pattern p))
			       :key (clone (key-pattern p))
			       :dur (clone (dur-pattern p))
			       :amp (clone (amp-pattern p)))))
    (dolist (key (property-keys p :local))
      (property! other key (property p key)))
    (setf other (funcall hook other))
    (add-child! new-parent other)
    other))

(defmethod transpose! ((qb qball)(x t) &key (range '(0 127)))
  (if (transposable-p qb)
      (transpose! (key-pattern qb) x :range range))
  qb)

(defmethod invert! ((qb qball)(pivot t) &key (range '(0 127)))
  (if (transposable-p qb)
      (invert! (key-pattern qb) pivot :range range))
  qb)


;;; ---------------------------------------------------------------------- 
;;;			     METRONOME 


(defun --metronome-key-list (bars beats)
  (let ((pat1 (cons 'phrase (copies (1- beats) 'beat)))
	(pat2 (cons 'accent (copies (1- beats) 'beat))))
    (flatten (cons pat1 (copies (1- bars) pat2)))))

(defmacro metronome (name &key
		      (instrument *metronome-instrument*)
		      (project *project*)
		      (section nil))
  "Creates QBALL for use as metronome.
   name       - Symbol, the QBALL is bound to name.
   instrument - Default *METRONOME-INSTRUMENT*
   project    - Default *PROJECT*
   section    - Default current section of project.

   The instrument keynumber-map should accept the following symbols:
   BEEP ACCENT and PHRASE.  The PHRASE tone is produced each time 
   the phrase is repeated.  The ACCENT tone is produced at the start
   of each bar.  The BEEP tone produced at all other beats."
  `(progn
     (validate-part-parents ,project ,section)
     (let* ((sec (or ,section (current-section ,project)))
     	    (bars (bar-count sec))
     	    (beats (beat-count sec))
     	    (cue-list (let ((acc '()))
     	    		(dotimes (br bars)
     	    		  (dotimes (bt beats)
     	    		    (push (list (1+ br)(1+ bt) 1) acc)))
     	    		(reverse acc)))
     	    (key-list (--metronome-key-list bars beats))
     	    (amp-list key-list)
     	    (prt (make-instance 'qball
     	    			:name ',name
     	    			:instruments (->list ,instrument)
     	    			:period (duration sec)
     	    			:cue (cycle :of cue-list)
     	    			:key (cycle :of key-list)
     	    			:dur (cycle :of '(0.01))
     	    			:amp (cycle :of amp-list))))
       (property! prt :qfn #'bar)
       (property! prt :keynumber-map #'identity)
       (property! prt :duration-map #'identity)
       (property! prt :amplitude-map #'identity)
       (add-child! sec prt)
       (property! prt :qfn #'bar)
       (param ,name prt)
       prt)))