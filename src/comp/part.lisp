;;;; cyco.comp.part
;;;;
;;;; PART is a super class for several other classes.   Typically an
;;;; instance of PART is not used directly, instead one of it's subclasses
;;;; is used.   In general a PART combines one or more INSTRUMENTs with
;;;; instructions on how to play specific events.
;;;;
;;;; PART class hierarchy:
;;;;
;;;; PART
;;;;  |
;;;;  +-- PROGRAMS
;;;;  +-- FIXED-PART
;;;;  +-- EPART
;;;;  +---STRUMMER
;;;;  +-- QBALL
;;;;  |    |
;;;;  |    +-- metronome (not an actual subclass of QBALL)
;;;;  |
;;;;  +-- CONTROLBALL
;;;;  |    |
;;;;  |    +-- PRESSUREBALL
;;;;  |    +-- BENDBALL
;;;;  |
;;;;  +-- MARKER
;;;;  

(in-package :cyco)

(defclass part (node)
  ((instruments
    :type t
    :accessor instruments
    :initform '()
    :initarg :instruments)
   (period
    :type float
    :accessor period
    :initform 1.0
    :initarg :period)
   (mute
    :type t
    :reader mute?
    :initform nil
    :initarg :mute)
   (events
    :type list
    :accessor events
    :initform '()
    :initarg :events)
   (transposable
    :type t
    :initform t
    :initarg :transposable)))

(defmethod part-p ((obj part)) obj)

(defmethod mute ((prt part)(flag t))
  (setf (slot-value prt 'mute) flag))

(defmethod mute ((pname symbol)(flag t))
  (let* ((sec (current-section *project*))
	 (prt (get-child sec pname)))
    (or (and prt (mute prt flag))
	(error (format nil "Current section ~A does not have part ~A"
		       (name sec) pname)))))

(defmethod solo ((prt part))
  "Unmute PART and simultaneously mute all other parts in the same SECTION."
  (dolist (other (cohorts prt))
    (mute other t))
  (mute prt nil)
  (maphash #'(lambda (key val)
	       (dismiss key)
	       (setf (group-state val) nil))
	   (groups (parent prt))))

(defmethod solo ((pname symbol))
  "Solo PART using part name."
  (let* ((sec (current-section *project*))
	 (prt (get-child sec pname)))
    (if prt
	(solo prt)
      (let ((frmt "Can not solo PART ~A in Section ~A"))
	(cyco-warning (format nil frmt pname (name sec)))))))

(defmethod dump ((prt part) &key (depth 0)(max-depth 10))
  (if (< depth max-depth)
      (if (mute? prt)
	  (format t "~AMUTED ~A ~A~%" (tab depth)(type-of prt)(name prt))
	(progn
	  (format t "~A~A ~A~%" (tab depth)(type-of prt)(name prt))
	  (setf (events prt)
		(sort (events prt)
		      #'(lambda (a b)(< (car a)(car b)))))
	  (dolist (evn (events prt))
	    (dump evn :depth (1+ depth) :max-depth max-depth)))))
  nil)

(defmethod render ((prt part) &key (repeat 1)(offset 0))
  (let ((p (period prt))
	(acc '()))
    (dotimes (i repeat)
      (setf acc (append acc (render-once prt :offset (+ offset (* i p))))))
    acc))

(defun validate-part-parents (project section)
  "Many cyco functions use the current SECTION of the global *PROJECT*.
   VALIDATE-PART-PARENTS test that these values are defined.
   If either *PROJECT* is not bound to a PROJECT or it does not have
   a current SECTION, an error is produced."
  (or (and (project-p project)
	   (section-p (or section (current-section project))))
      (let ((frmt "Either project or section is not defined"))
	(cyco-warning (format nil "project ~A" project))
	(cyco-warning (format nil "section ~A" section))
	(error frmt))))

(defun ->instrument-list (instruments project)
  "Converts list of INSTRUMENT names to a list of corresponding 
   INSTRUMENTs.

   The instruments argument may contain either actual instruments or
   symbols which name instruments." 
  (let ((acc '())
	(orc (property project :orchestra)))
    (dolist (i (->list instruments))
      (if (instrument-p i)
	  (push i acc)
	(push (find-node orc i) acc)))
    acc))

(defmethod clone ((p part) &key
		  (newname "~A-clone")
		  (parent nil)
		  (hook #'identity))
  (let* ((new-name (format nil newname (name p)))
	 (new-parent (or parent (parent p)))
	 (other (make-instance 'part
			       :parent new-parent
			       :name new-name
			       :instruments (instruments p)
			       :period (period p)
			       :mute (mute? p)
			       :events (clone (events p)))))
    (dolist (key (property-keys p :local))
      (property! other key (property p key)))
    (setf other (funcall hook other))
    (add-child! new-parent other)
    other))

(defmethod transpose! ((p part)(x fixnum) &key (range '(0 127)))
  (if (transposable-p p)
      (let ((acc '()))
	(dolist (e (events p))
	  (push (cons (car e)(transpose (cdr e) x :range range)) acc))
	(setf (events p)(reverse acc))))
  p)

(defmethod invert! ((p part)(pivot t) &key (range '(0 127)))
  (if (transposable-p p)
      (let ((acc '()))
	(dolist (e (events p))
	  (push (cons (car e)(invert (cdr e) pivot :range range)) acc))
	(setf (events p)(reverse acc))))
  p)

(defmethod transposable-p ((prt part))
  (slot-value prt 'transposable))

(defmethod dump-events ((p part) &key (filter nil)(time (cons 0 1e6)))
  (dump-events (render-once p) :filter filter :time time))

