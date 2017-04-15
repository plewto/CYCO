;;;; cyco.comp.controlball
;;;;
;;;; CONTROLBALL and it's two sub-classes PRESSUREBALL and BENDBALL,
;;;; are PARTs which generate MIDI controller, pressure and bend events
;;;; respectively using PATTERNs.   CONTROLBALL has the same basic structure as
;;;; QBALL.
;;;;

(in-package :cyco)

(defclass controlball (part)
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
   (controller-number          		; MIDI controller number (0..127).
    :type t      			; :BEND for BENDBALL
    :accessor controller-number		; :PRESSURE for PRESSUREBALL
    :initform 1				
    :initarg :ctrl)
   (value-pattern
    :type pattern
    :accessor value-pattern
    :initform (cycle :of 0)
    :initarg :values)))

(defmacro controlball (name ctrl instruments &key
			    (cue '())
			    (values 0)
			    (period nil)
			    (qfn #'bar)
			    (project *project*)
			    (section nil))
  "Bind new CONTROLBALL to name.
   name        - Symbol
   ctrl        - MIDI controller number or a controller alias.
   instruments - Instrument or list of instruments.
   :cue        - List of time cue events.  See QBALL
   :values     - List of controller values or PATTERN of values.
                 If values is a list it is converted to a CYCLE. 
                 All values are 'normalized' between 0 and 1 inclusive.
   :period     - Period of this part in seconds, defaults to duration 
                 of parent section.
   :qfn        - Cuing function, default #'BAR
   :project    - Default *PROJECT*
   :section    - Defaults to current section of project."
  `(progn
     (validate-part-parents ,project ,section)
     (let* ((sec (or ,section (current-section ,project)))
	    (ilist (->instrument-list ,instruments ,project))
	    (cmap (property ,project :controller-assignments))
	    (prt (make-instance 'controlball
				:name ',name
				:instruments ilist
				:transposable nil
				:cue-fn ,qfn
				:ctrl (controller-assignment ,ctrl :cmap cmap)
				:period (or ,period (duration sec))
				:cue (cycle :of ,cue)
				:values (->pattern ,values))))
       (add-child! sec prt)
       (param ,name prt)
       prt)))

(defmethod reset ((obj controlball))
  (reset (cue-pattern obj))
  (reset (value-pattern obj))
  obj)

(defmethod dump ((prt controlball) &key (depth 0)(max-depth 10))
  (if (< depth max-depth)
      (if (mute? prt)
	  (format t "~AMUTED ~A ~A ~A~%"
		  (tab depth)(type-of prt)
		  (name prt)(controller-number prt))
	(let ((pad (tab (1+ depth))))
	  (format t "~A ~A ~A ~A~%" 
		  (tab depth) (type-of prt)
		  (name prt)(controller-number prt))
	  (if (< (1+ depth) max-depth)
	      (progn
		(format t "~ACUE: ~A~%" pad (->string (cue-pattern prt)))
		(format t "~AVAL: ~A~%" pad (->string (value-pattern prt))))))))
  nil)

(defmethod render-once ((prt controlball) &key (offset 0))
  (reset prt)
  (if (not (mute? prt))
      (let ((acc '())
	    (qfn (cue-fn prt))
	    (ctrl (controller-number prt))
	    (cuepat (cue-pattern prt))
	    (valpat (value-pattern prt))
	    (proj (parent (parent prt))))
	(setf (current-section proj)(parent prt))
	(dotimes (i (cardinality cuepat))
	  (let ((time (+ offset (apply qfn (next-1 cuepat))))
		(val (truncate (* 127 (next-1 valpat)))))
	    (dolist (inst (instruments prt))
	      (let ((cindex (1- (channel inst :resolve t))))
		(push (cons time (midi-control-change cindex ctrl val)) acc)))))
	acc)
    nil))
				
(defmethod clone ((p controlball) &key
		  (newname "~A-clone")
		  (parent nil)
		  (hook #'identity))
  (let* ((new-name (format nil newname (name p)))
	 (new-parent (or parent (parent p)))
	 (other (make-instance (type-of p)
			       :parent new-parent
			       :name new-name
			       :instruments (instruments p)
			       :period (period p)
			       :mute (mute? p)
			       :cue-fn (cue-fn p)
			       :cue (clone (cue-pattern p))
			       :ctrl (controller-number p)
			       :values (clone (value-pattern p)))))
    (dolist (key (property-keys p :local))
      (property! other key (property p key)))
    (setf other (funcall hook other))
    (add-child! new-parent other)
    other))
		
(defmethod transposable-p ((cb controlball)) nil)

(defmethod transpose! ((cb controlball)(n t) &key (range :ignore))
  (dismiss range)
  cb)

(defmethod invert! ((cb controlball)(n t) &key (range :ignore))
  (dismiss range)
  cb)



;;; ---------------------------------------------------------------------- 
;;;			    PRESSUREBALL class

(defclass pressureball (controlball) nil)

(defmacro pressureball (name instruments &key
			     (cue '())
			     (values 0)
			     (period nil)
			     (qfn #'bar)
			     (project *project*)
			     (section nil))
   "Bind new PRESSUREBALL to name.
   name        - Symbol
   instruments - Instrument or instrument name, or list of instruments
                 and names.
   :cue        - List of time cue events.  See QBALL
   :values     - List of controller values or PATTERN of values.
                 If values is a list it is converted to a CYCLE. 
                 All values are 'normalized' between 0 and 1 inclusive.
   :period     - Period of this part in seconds, defaults to duration 
                 of parent section.
   :qfn        - Cuing function, default #'BAR
   :project    - Default *PROJECT*
   :section    - Defaults to current section of project."
  `(progn
     (validate-part-parents ,project ,section)
     (let* ((sec (or ,section (current-section ,project)))
	    (ilist (->instrument-list ,instruments ,project))
	    (cmap (property ,project :controller-assignments))
	    (prt (make-instance 'pressureball
				:name ',name
				:instruments ilist
				:ctrl :pressure
				:period (or ,period (duration sec))
				:cue (cycle :of ,cue)
				:values (->pattern ,values))))
       (add-child! sec prt)
       (property! prt :qfn ,qfn)
       (param ,name prt)
       prt)))

(defmethod render-once ((prt pressureball) &key (offset 0))
  (reset prt)
  (if (not (mute? prt))
      (let ((acc '())
	    (qfn (cue-fn prt))
	    (cuepat (cue-pattern prt))
	    (valpat (value-pattern prt))
	    (proj (parent (parent prt))))
	(setf (current-section proj)(parent prt))
	(dotimes (i (cardinality cuepat))
	  (let ((time (+ offset (apply qfn (next-1 cuepat))))
		(val (truncate (* 127 (next-1 valpat)))))
	    (dolist (inst (instruments prt))
	      (let ((cindex (1- (channel inst :resolve t))))
		(push (cons time (midi-channel-pressure cindex val)) acc)))))
	acc)
    nil))

;;; ---------------------------------------------------------------------- 
;;;			      BENDBALL class

(defclass bendball (controlball) nil)

(defmacro bendball (name instruments &key
			 (cue '())
			 (values 0)
			 (period nil)
			 (qfn #'bar)
			 (project *project*)
			 (section nil))
  "Bind new BENDBALL to name.
   name        - Symbol
   instruments - Instrument or instrument name, or list of instruments
                 and names.
   :cue        - List of time cue events.  See QBALL
   :values     - List of controller values or PATTERN of values.
                 If values is a list it is converted to a CYCLE. 
                 All values are 'normalized' between -1 and +1 inclusive.
   :period     - Period of this part in seconds, defaults to duration 
                 of parent section.
   :qfn        - Cuing function, default #'BAR
   :project    - Default *PROJECT*
   :section    - Defaults to current section of project."
  `(progn
     (validate-part-parents ,project ,section)
     (let* ((sec (or ,section (current-section ,project)))
	    (ilist (->instrument-list ,instruments ,project))
	    (cmap (property ,project :controller-assignments))
	    (prt (make-instance 'bendball
				:name ',name
				:instruments ilist
				:ctrl :bend
				:period (or ,period (duration sec))
				:cue (cycle :of ,cue)
				:values (->pattern ,values))))
       (add-child! sec prt)
       (property! prt :qfn ,qfn)
       (param ,name prt)
       prt)))

(defmethod render-once ((prt bendball) &key (offset 0))
  (reset prt)
  (if (not (mute? prt))
      (let ((acc '())
	    (qfn (cue-fn prt))
	    (cuepat (cue-pattern prt))
	    (valpat (value-pattern prt))
	    (proj (parent (parent prt))))
	(setf (current-section proj)(parent prt))
	(dotimes (i (cardinality cuepat))
	  (let* ((time (+ offset (apply qfn (next-1 cuepat))))
		 (v (bend->midi-data (next-1 valpat)))
		 (lsb (aref v 0))
		 (msb (aref v 1)))
	    (dolist (inst (instruments prt))
	      (let ((cindex (1- (channel inst :resolve t))))
		(push (cons time (midi-pitch-bend cindex lsb msb)) acc)))))
	acc)
    nil))
