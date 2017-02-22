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
   TODO: change doc for instrument pattern

   Binds new instance of QBALL to name.
   A QBALL is a PART which uses multiple patterns to generate MIDI note events.
   For each event, a new combination of pattern values are combined to 
   produce note parameters.   The patterns are:
       :qfn - cueing function
       :cue - cue values 
       :key - key numbers
       :dur - duration
       :amp - amplitude

   The default pattern type for all parameters is a CYCLE.
   If the lengths of the various patterns are different, complex combinations 
   of the parameters are produced.

   name        - Symbol, the QBALL is bound to name.

   instruments - Instrument pattern may be either a single instrument, a list 
                 of instruments or a PATTERN of instruments.
                   single instrument      - plays all events.
                   list of instruments    - all notes plays by all instruments
                                            in a layer.
                   pattern of instruments - Specific instrument is selected 
                                            for each note as per the pattern
                                            pattern type.

   :cue        - List of time specifications.  The format is a nested list
                 ((time-1)(time-2)...(time-n))  where (time-i) must be a form
                 expected by the qfn.  For the default qfn #'BAR time expressions 
                 should be (bar beat subbeat tick).
   :period     - Duration of this PART in seconds.  The default is to inherit
                 the duration of the parent SECTION.  If period is less then 
                 the SECTION duration, then the QBALL will be repeated as 
                 needed.

   :qfn        - Function, list of functions or pattern of functions.
                 The cueing function.  The values contained in the cue list 
                 *MUST* match the expected format of the cuing function.

   :transposable - Boolean, if nil transpose! and invert! have no effect.
                 It is often useful to set percussion parts to be non
                 transposable.
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
     (let* ((ipat (cond ((listp ,instruments)
			 (cycle :of (list ,instruments)))
			((pattern-p ,instruments)
			 ,instruments)
			(t (cycle :of (->list ,instruments)))))
	    (sec (or ,section (current-section ,project)))
	    (prt (make-instance 'qball
				:name ',name
				:instruments ipat
				:period (or ,period (duration sec))
				:cue-fn (->pattern ,qfn)
				:cue (cycle :of ,cue)
				:key (->pattern ,key)
				:dur (->pattern ,dur)
				:amp (->pattern ,amp))))
       (add-child! sec prt)
       (property! prt :keynumber-map ,key-map)
       (property! prt :duration-map ,dur-map)
       (property! prt :amplitude-map ,amp-map)
       (property! prt :transposable ,transposable)
       (param ,name prt)
       (part-banner sec prt)
       prt)))

(defmethod reset ((obj qball))
  (reset (cue-pattern obj))
  (reset (key-pattern obj))
  (reset (dur-pattern obj))
  (reset (amp-pattern obj))
  (reset (cue-fn obj))
  obj)

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
	    (kmap (property prt :keynumber-map))
	    (dmap (property prt :duration-map))
	    (amap (property prt :amplitude-map))
	    (dscale (beat-duration (parent prt)))
	    (cuepat (cue-pattern prt)))
	(dotimes (i (cardinality cuepat))
	  (let* ((qfn (next-1 (cue-fn prt)))
		 (time1 (+ offset (apply qfn (->list (next-1 cuepat)))))
		 (k (funcall kmap (next-1 (key-pattern prt))))
		 (d (funcall dmap (next-1 (dur-pattern prt))))
		 (a (funcall amap (next-1 (amp-pattern prt))))
		 (ilist (->list (next-1 (instruments prt)))))
	    (dolist (inst ilist)
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

