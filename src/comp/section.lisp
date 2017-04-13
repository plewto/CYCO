;;;; cyco.comp.section
;;;;
;;;; A PROJECT is composed of a series of SECTIONS.
;;;; Each SECTION is composed of PARTS.
;;;;
;;;; A SECTION may be "skiped", in which case it is skiped over when the
;;;; project is rendered.  A SECTION has a "current" PART. 
;;;;

(in-package :cyco)

(defclass section (timesig)
  ((skip
    :type t
    :reader skip?
    :initform nil
    :initarg :skip)
   (current-part
    :type t
    :accessor current-part
    :initform nil)
   (groups				; Used to group parts
    :type hash-table			; together for mute
    :accessor groups			; and solo functions.
    :initform (make-hash-table))))
    

(defmethod section-p ((s section)) s)

(defmacro section (name &key
			(parent *project*)
			(tempo nil)
			(unit nil)
			(bars nil)
			(beats nil)
			(subbeats nil)
			(tsubbeats nil))
  "Creates new SECTION and bind it to the symbol name.
   name - symbol
   :parent - parent PROJECT, default *PROJECT*.  The 
             new section becomes the 'current' section of the parent.
   :tempo  - tempo in BPM, if not specified inherit from parent.
   :unit   - beat unit symbol, one of W H Q E or S. If not specified
             inherit from parent.
   :bars   - Length of section in bars.
   :beats  - Time signature beat count.
   :subbeats  - Number of subbeats per beat.
   :tsubbeats - Number of tsubbeats per beat"
  `(if (project-p ,parent)
       (let ((tsig (project-timesig ,parent))
   	     (s (make-instance 'section
			       :parent ,parent
			       :transient t
			       :name ',name)))
	 (add-child! ,parent s)
	 (property! s :unit (or ,unit (unit tsig)))
	 (property! s :bars (or ,bars (bar-count tsig)))
	 (property! s :beats (or ,beats (beat-count tsig)))
	 (property! s :subbeats (or ,subbeats (subbeat-count tsig)))
	 (property! s :tsubbeats (or ,tsubbeats (tsubbeat-count tsig)))
	 (property! s :count-ticks (property tsig :count-ticks))
	 (property! s :transposable t)
	 (tempo! s (or ,tempo (tempo tsig)))
	 (setf (current-section ,parent) s)
	 (marker (format nil "START SECTION ~a" ',name
	 		 :time 0
	 		 :qfn #'(lambda (_) -0.0001)
	 		 :period (duration s)
	 		 :project ,parent
	 		 :section s))
	 (param ,name s)
	 (section-banner s)
	 s)
     (error "No current project")))

(defmethod dump ((s section) &key (depth 0)(max-depth 10))
  (if (< depth max-depth)
      (if (skip? s)
	  (format t "~ASKIPPED SECTION ~A~%" (tab depth)(name s))
	(progn
	  (format t "~ASECTION ~A  [~A]~%" (tab depth)(name s)(rep-timesig s))
	  (dolist (c (reverse (children s)))
	    (dump c :depth (1+ depth) :max-depth max-depth)))))
  nil)

(defmethod dump-events ((s section) &key (filter nil)(time (cons 0 1e6)))
  (dump-events (render-once s) :filter filter :time time))

(defmethod add-child! ((s section)(n node) &key (test :ignore))
  "Adds a child node to the section, but only if it is a PART.
   The test predicate does not allow duplicate parts to be added."
  (dismiss test)
  (if (part-p n)
      (progn
	(call-next-method)
	(setf (current-part s) n)
	n)
    (let ((frmt "Can not add non-part ~A to section  ~A"))
      (error (format nil frmt n (name s))))))
				    
(defmethod duration ((s section))
  (or (and (skip? s) 0)
      (phrase-duration s)))

(defmethod render ((s section) &key (repeat 1)(offset 0))
  (let ((start offset)
	(acc '())
	(dur (duration s)))
    (dotimes (i repeat)
      (setf acc (append acc (render-once s :offset start)))
      (setf start (+ start dur)))
    (sort acc #'sort-midi-events-test)))


(defmethod ->midi ((sec section) &key filename (offset 0)(repeat 1)(pad-end 2))
  (let* ((smf (smf :initial-tempo (tempo sec)))
	 (trk (car (smf-tracks smf)))
	 (fname (str+ (or filename (string-downcase (name sec))) ".mid"))
	 (fqn (join-path (property sec :midi-directory) fname))
	 (events (render sec :repeat repeat :offset offset)))
    (dolist (ev events)
      (push-event (car ev)(cdr ev) trk))
    (save-smf smf fqn :pad-end pad-end)
    events))

(defun bars (n)
  "Returns duration of n bars."
  (let* ((sec (current-section *project*)))
    (* n (bar-duration sec))))

(defun bar (&optional (bar 1)(beat 1)(sub 1)(tsub nil)(tick 0))
  "BAR is the default time cuing function used by many types of PART.
   bar  - bar number 1,2,3,... Default 1
   beat - beat within bar  (for 4/4 time 1,2,3 or 4) Default 1
   sub  - subbeat within beat (for 4 sixteenth notes per beat  1,2,3 or 4)
          Default 1
   tsub - flag, if true interpret sub as tsubbeats, 
   tick - Adds/removes duration of specified number of ticks. Default 0."
  (let* ((sec (current-section *project*)))
    (+ (* (max 0 (1- bar))(bar-duration sec))
       (* (max 0 (1- beat))(beat-duration sec))
       (* (max 0 (1- sub))(if tsub
			      (tsubbeat-duration sec)
			    (subbeat-duration sec)))
       (* tick (tick-duration sec)))))

(defun tbar (&optional (bar 1)(beat 1)(tsubbeat 1)(tick 0))
  "TBAR is an alternate cuing function to BAR.  It has the same
   usage as BAR except that the subbeat value is replaced by tsubbeat."
  (let* ((sec (current-section *project*)))
    (+ (* (max 0 (1- bar))(bar-duration sec))
       (* (max 0 (1- beat))(beat-duration sec))
       (* (max 0 (1- tsubbeat))(tsubbeat-duration sec))
       (* tick (tick-duration sec)))))

(defun skip (flag &optional (section-name nil))
  "Set skip flag of indicated SECTION.
   If section-name is not specified use current section of *PROJECT*."
  (let ((sec (if section-name
		 (get-child *project* section-name)
	       (current-section *project*))))
    (setf (slot-value sec 'skip) flag)
    sec))
      
;; (defmethod render-once ((s section) &key (offset 0))
;;   (if (not (skip? s))
;;       (let ((acc '())
;; 	    (dur (duration s)))
;; 	(dolist (c (children s))
;; 	  (let* ((pdur (period c))
;; 		 (cnt (truncate (/ dur pdur))))
;; 	    (dotimes (i cnt)
;; 	      (setf acc (append acc (render-once c :offset (+ offset (* i pdur))))))))
;; 	(sort acc #'(lambda (a b)(< (car a)(car b)))))
;;     nil))

(defmethod render-once ((s section) &key (offset 0))
  (if (not (skip? s))
      (let ((acc '())
	    (dur (duration s)))
	(dolist (prt (children s))
	  (let* ((pdur (period prt))
		 (cnt (truncate (/ dur pdur))))
	    (dotimes (i cnt)
	      (setf acc (append acc (render-once prt :offset (+ offset (* i pdur))))))))
	(sort acc #'(lambda (a b)(< (car a)(car b)))))
    nil))

;; NOTE: SECTION groups field is not preserved by cloning.
;;
(defmethod clone ((s section) &key
		  (newname "~A-clone")
		  (parent nil)
		  (hook #'identity))
  (let* ((new-name (format nil newname (name s)))
	 (new-parent (or parent (parent s)))
	 (other (make-instance 'section
			       :parent new-parent
			       :name new-name
			       :transient t
			       :skip (skip? s))))
    (dolist (key (property-keys s :local))
      (property! other key (property s key)))
    (dolist (p (children s))
      (clone p :parent other))
    (setf (current-part other)(final (children other)))
    (setf other (funcall hook other))
    (add-child! new-parent other)
    other))
	    
(defmethod transpose! ((s section)(x t) &key (range '(0 127)))
  (if (transposable-p s)
      (dolist (c (children s))
	(transpose! c x :range range)))
  s)

(defmethod invert! ((s section)(pivot t) &key (range '(0 127)))
  (if (transposable-p s)
      (dolist (c (children s))
	(invert! c pivot :range range)))
  s)

