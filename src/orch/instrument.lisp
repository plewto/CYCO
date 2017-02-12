;;;; cyco.orch.instrument
;;;;
;;;; An INSTRUMENT is a specialized NODE used as a proxy for some external
;;;; MIDI device.  A tree of instruments define a project's "orchestra". 
;;;;
;;;; Instruments are constructed hierarchically. A typical example would be a
;;;; drum machine which has several "sub: instruments (kick, snare, tom ...)
;;;;
;;;; Usually a base level orchestra is described in the configuration files
;;;; to match the local environment.  At the same time specialized functions
;;;; can be defined for adding mote specific instruments as needed for a
;;;; composition.
;;;;
;;;; Each INSTRUMENT either defines, or inherits from it's parent, one of
;;;; several mapping functions.
;;;;
;;;;     keynumber-map  maps keynumber n to n'
;;;;     duration-map   maps note duration d to d'
;;;;     amplitude-map  maps amplitude a to a'
;;;;     program-change-hook  converts program-change events to MIDI events.
;;;;
;;;; Each PROJECT receives a copy of the global *ROOT-INSTRUMENT* variable
;;;; on which it builds it's specific orchestra.
;;;;

(in-package :cyco)

(defgeneric instrument-keynumber (inst keynum))
(defgeneric instrument-duration (inst n))
(defgeneric instrument-amplitude (inst a))
(defgeneric instrument-velocity (inst a))

(defclass instrument (node) nil)

(defmethod instrument-p ((obj instrument)) obj)

(defun default-duration-map (dur)
  (metric dur))

(defun default-amplitude-map (amp)
  (amplitude amp))

(defun create-instrument (name &key
			       (parent *root-instrument*)
			       (transient t)
			       (channel-assignments nil)
			       (controller-assignments nil)
			       (channel nil)
			       (keynumber-map nil)
			       (duration-map nil)
			       (amplitude-map nil)
			       (program-change-hook nil)
			       (program-change-offset nil) ; time of event offset 
			       (program-number 0)
			       (program-bank 0)
			       (remarks nil))
  "Creates new instance of INSTRUMENT class.
   parent - The parent instrument, default *ROOT-INSTRUMENT*
   transient - Boolean, if an instrument is marked as transient it is 
               removed from the orchestra tree by the free-orchestra! 
               method.   Non transient instruments are note so removed
               and typically represent physical devices.
   channel-assignments - instance of  CHANNEL-ASSIGNMENT-MAP
              defaults to nil (*GLOBAL-CHANNEL-ASSIGNMENTS* from parent)
   controller-assignments - instance of CONTROLLER-ASSIGNMENT-MAP
              defaults to nil (*GLOBAL-CONTROLLER-ASSIGNMENTS* from parent)
   keynumber-map - See orch/keymaps.lisp
              defaults to nil (DEFAULT-KEYMAP from parent)
   duration-map - defaults to nil (DEFAULT-DURATION-MAP from parent)
   amplitude-map - defaults to nil (DEFAULT-AMPLITUDE-MAP from parent)
   program-change-hook defaults to nil (DEFAULT-PROGRAM-CHANGE-HOOK from parent)
   program-change-offset - nil, offset time for program change events.
   program-number - nil, 
   program-bank - nil
   remarks - optional text displayed as part of help message."
  (let ((inst (make-instance 'instrument
			     :name name
			     :parent parent
			     :transient transient)))
    (property! inst :channel-assignments channel-assignments)
    (property! inst :controller-assignments controller-assignments)
    (property! inst :keynumber-map keynumber-map)
    (property! inst :duration-map duration-map)
    (property! inst :amplitude-map amplitude-map)
    (property! inst :program-change-hook program-change-hook)
    (property! inst :program-change-offset program-change-offset)
    (property! inst :channel channel)
    (property! inst :program-number program-number)
    (property! inst :program-bank program-bank)
    (remarks! inst remarks)
    (add-child! parent inst)
    inst))

(setf *root-instrument*
      (create-instrument 
       :root
       :transient nil
       :channel-assignments *global-channel-assignments*
       :controller-assignments *global-controller-assignments*
       :parent nil
       :channel 1
       :keynumber-map #'default-keymap
       :duration-map #'default-duration-map
       :amplitude-map #'default-amplitude-map
       :program-change-hook #'default-program-change-hook))

(defun free-orchestra! (&key (node *project*)(force nil))
  "Remove all non-transient instruments from orchestra.
   node - default *PROJECT*
   force - Boolean, if true remove ALL instruments from orchestra
           tree. Default nil"
  (let ((n (cond ((instrument-p node) node)
		 ((project-p node)(orchestra :project node))
		 (t nil))))
    (and n (prune-tree! n :force force))))

(defmethod channel ((inst instrument) &key (resolve t))
  (let ((cmap (property inst :channel-assignments
			:default *global-channel-assignments*))
	(c (property inst :channel :default 1)))
    (channel-assignment c :cmap cmap :resolve resolve)))

(defmethod channel! ((inst instrument)(c t))
  (property! inst :channel c))

(defmethod instrument-keynumber ((inst instrument)(keynum t))
  (let ((kmap (property inst :keynumber-map)))
    (funcall kmap (keynumber keynum))))

(defmethod instrument-keynumber ((inst instrument)(lst list))
  (let ((kmap (property inst :keynumber-map)))
    (mapcar #'(lambda (n)(funcall kmap (keynumber n))) lst)))

(defmethod instrument-duration ((inst instrument)(n t))
  (let ((dmap (property inst :duration-map)))
    (funcall dmap (metric n))))

(defmethod instrument-duration ((inst instrument)(lst list))
  (let ((dmap (property inst :duration-map)))
    (mapcar #'(lambda (n)(funcall dmap (metric n))) lst)))
	
(defmethod instrument-amplitude ((inst instrument)(a t))
  (let ((amap (property inst :amplitude-map)))
    (funcall amap (amplitude a))))

(defmethod instrument-amplitude ((inst instrument)(lst list))
  (let ((amap (property inst :amplitude-map)))
    (mapcar #'(lambda (n)(funcall amap (amplitude n))) lst)))

(defmethod instrument-velocity ((inst instrument)(amp t))
  (max -1 (truncate (* 127 (instrument-amplitude inst amp)))))

(defmethod instrument-velocity ((inst instrument)(lst list))
  (let ((alst (instrument-amplitude inst lst))
	(acc '()))
    (dolist (a alst)
      (push (instrument-velocity inst a) acc))
    (reverse acc)))

(defmethod dump ((inst instrument) &key (depth 0)(max-depth 10))
  (if (< depth max-depth)
      (progn 
	(format t "~A~A (channel ~A)~%"
		(tab depth)(name inst)(channel inst :resolve t))
	(dolist (c (reverse (children inst)))
	  (dump c :depth (1+ depth) :max-depth max-depth))))
  nil)
