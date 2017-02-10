;;;; cyco.midi.event
;;;;
;;;; Defines MIDI event classes
;;;;
;;;;  MIDI-EVENT
;;;;   |
;;;;   +-- CHANNEL-EVENT    KEY-EVENT
;;;;   |    |                |
;;;;   |    |    +-----------+
;;;;   |    |    |
;;;;   |    +-- NOTE-OFF
;;;;   |    +-- NOTE-ON
;;;;   |    +-- POLY-PRESSURE
;;;;   |    |
;;;;   |    +-- CONTROL-CHANGE
;;;;   |    +-- PITCH-BEND
;;;;   |    +-- CHANNEL-PRESSURE
;;;;   |    +-- PROGRAM-CHANGE
;;;;   |
;;;;   +-- SYSTEM-COMMON-EVENT
;;;;   |    |
;;;;   |    +-- SYSTEM-EXCLUSIVE-EVENT
;;;;   |    +-- END-SYSTEM-EXCLUSIVE-EVENT
;;;;   |
;;;;   +-- META-EVENT
;;;;        |
;;;;        +-- META-TEXT-EVENT
;;;;        +-- END-OF-TRACK-EVENT
;;;;        +-- TEMPO-CHANGE-EVENT
;;;;        +-- TIMESIG-EVENT
;;;;


(in-package :cyco)

;;; ---------------------------------------------------------------------- 
;;;			     MIDI-EVENT class

(defclass midi-event nil
  ((command				; MIDI status command
    :type fixnum			; defined in constants.
    :reader command
    :initarg :command)
   (priority				; Sort order priority for events
    :type fixnum			; with identical time. 
    :reader priority			; Lower value priorities 
    :initform 0				; appear before events with higher
    :initarg :priority)))		; priority values.

(defmethod mnemonic ((evn midi-event))
  (gethash (command evn) +MNEMONICS+))

(defmethod ->string ((evn midi-event))
  (format nil "~A " (mnemonic evn)))

(defmethod midi-event-p ((obj midi-event)) obj)



;;; ---------------------------------------------------------------------- 
;;;			    CHANNEL-EVENT class

(defclass channel-event (midi-event)
  ((channel-index			; MIDI channel-index
    :type fixnum			; between 0..15 inclusive.
    :accessor channel-index
    :initform 0
    :initarg :channel-index)
   (data1				; MIDI data value
    :type fixnum			; 0..127 inclusive.
    :initform 0
    :accessor data1
    :initarg :data1)
   (data2
    :type fixnum
    :initform 0
    :accessor data2
    :initarg :data2)))

(defmethod channel-event-p ((obj channel-event)) obj)

(defmethod data-count ((evn channel-event)) 2)

(defmethod data! ((evn channel-event)(index fixnum)(value fixnum))
  (cond ((eq index 1)
	 (setf (data1 evn)(validate-data value)))
	((eq index 2)
	 (setf (data2 evn)(validate-data value)))
	(t (let ((msg (format nil "Invalid index to data!, expected 1 or 2, encountered ~A" index)))
	     (error msg)))))
	
(defun validate-channel-index (n)
  (logand n #xF))

(defun validate-data (n)
  (limit n 0 127))

(defmethod ->string ((evn channel-event))
  (let* ((dcount (data-count evn))
	 (acc (str+ (call-next-method)
		  (format nil "cindex: ~x   " (channel-index evn)))))
    (if (= dcount 1)
	(str+ acc
	      (format nil "data: ~2X" (data1 evn)))
      (str+ acc
	    (format nil "data: ~2X ~2X" (data1 evn)(data2 evn))))))

(defmethod render-event ((evn channel-event))
  (let ((status (+ (command evn)(channel-index evn))))
    (if (= (data-count evn) 2)
	(list status (data1 evn)(data2 evn))
      (list status (data1 evn)))))

(defmethod clone ((ev channel-event) &key newname parent (hook #'identity))
  (let ((other (make-instance (type-of ev)
			      :channel-index (channel-index ev)
			      :data1 (data1 ev)
			      :data2 (data2 ev)
			      :command (command ev)
			      :priority (priority ev))))
    (funcall hook other)))

;;; ---------------------------------------------------------------------- 
;;;			      KEY-EVENT classes

(defclass key-event nil nil)

(defmethod key-event-p ((obj key-event)) obj)

(defmethod keynumber ((obj key-event))
  (data1 obj))

(defmethod keyname ((obj key-event))
  (keyname (data1 obj)))

(defmethod pitch-class ((obj key-event))
  (pitch-class (data1 obj)))

(defmethod octave ((obj key-event))
  (octave (data1 obj)))

(defun validate-keynumber (kn)
  (while (minusp kn)(setf kn (+ 12 kn)))
  (while (> kn 127)(setf kn (- kn 12)))
  kn)

(defclass note-off (channel-event key-event)
  ((command
    :type fixnum
    :initform +NOTE-OFF+)
   (priority
    :initform 13)))

(defclass note-on (channel-event key-event)
  ;; NOTE: NOTE-ON event with velocity 0 is converted to NOTE-OFF.
  ((command
    :type fixnum
    :initform +NOTE-ON+)
   (priority
    :initform 11)))

(defclass poly-pressure (channel-event key-event)
  ;; POLY-PRESSURE included for compleatnes, but is otherwise not
  ;; supported by cyco.
  ((command
    :type fixnum
    :initform +POLY-PRESSURE+)
   (priority
    :initform 12)))

(defmethod pressure-event-p ((obj poly-pressure)) obj)

(defun midi-note-off (channel-index keynumber velocity)
  (make-instance 'note-off
		 :channel-index (validate-channel-index channel-index)
		 :data1 (validate-keynumber keynumber)
		 :data2 (validate-data velocity)))

(defun midi-note-on (channel-index keynumber velocity)
  (if (zerop velocity)
      (midi-note-off channel-index keynumber 0)
    (make-instance 'note-on
		   :channel-index (validate-channel-index channel-index)
		   :data1 (validate-keynumber keynumber)
		   :data2 (validate-data velocity))))

(defun midi-poly-pressure (channel-index keynumber pressure)
  (make-instance 'poly-pressure
		 :channel-index (validate-channel-index channel-index)
		 :data1 (validate-keynumber keynumber)
		 :data2 (validate-data pressure))) 

(defmethod transpose ((obj key-event)(x t) &key (range '(0 127)))
  (clone obj :hook #'(lambda (n)
		       (setf (slot-value n 'data1)
			     (transpose (slot-value n 'data1) x
					:range range))
		       n)))

;; (defmethod inversion ((obj key-event)(pivot t) &key (range '(0 127)))
;;   (clone obj :hook #'(lambda (n)
;; 		       (setf (slot-value n 'data1)
;; 			     (inversion (slot-value n 'data1) pivot
;; 					:range range))
;; 		       n)))

(defmethod invert ((obj key-event)(pivot t) &key (range '(0 127)))
  (clone obj :hook #'(lambda (n)
		       (setf (slot-value n 'data1)
			     (invert (slot-value n 'data1) pivot
				     :range range))
		       n)))

;;; ---------------------------------------------------------------------- 
;;;			   CONTROL-CHANGE class

(defclass control-change (channel-event)
  ((command
    :type fixnum
    :initform +CONTROL-CHANGE+)
   (priority
    :initform 12)))

(defmethod control-event-p ((obj control-change)) obj)

(defun midi-control-change (channel-index controller-number value)
  (make-instance 'control-change
		 :channel-index (validate-channel-index channel-index)
		 :data1 (validate-data controller-number)
		 :data2 (validate-data value)))

;;; ---------------------------------------------------------------------- 
;;;			     PITCH-BEND class
;;;
;;; MIDI pitch-bend values are stored as two 7-bit bytes LSB, HSB.
;;; See util/midi-utilities for functinos converting between "normalized"
;;; bend values and 2-byte form.

(defclass pitch-bend (channel-event)
  ((command
    :type fixnum
    :initform +PITCH-BEND+)
   (priority
    :initform 12)))

(defmethod bend-event ((obj pitch-bend)) obj)

(defun midi-pitch-bend (channel-index lsb msb)
  (make-instance 'pitch-bend
		 :channel-index (validate-channel-index channel-index)
		 :data1 (validate-data lsb)
		 :data2 (validate-data msb)))

;;; ---------------------------------------------------------------------- 
;;;			  CHANNEL-PRESSURE class

(defclass channel-pressure (channel-event)
  ((command
    :type fixnum
    :initform +CHANNEL-PRESSURE+)
   (priority
    :initform 12)))

(defmethod pressure-event-p ((obj channel-pressure)) obj)

(defun midi-channel-pressure (channel-index pressure)
  (make-instance 'channel-pressure
		 :channel-index (validate-channel-index channel-index)
		 :data1 (validate-data pressure)
		 :data2 0))

(defmethod data-count ((evn channel-pressure)) 1)

;;; ---------------------------------------------------------------------- 
;;;			  PROGRAM-CHANGE class

(defclass program-change (channel-event)
  ((command
    :type fixnum
    :initform +PROGRAM-CHANGE+)
   (priority
    :initform 10)))

(defmethod program-event-p ((obj program-change)) obj)

(defun midi-program-change (channel-index program-number)
  (make-instance 'program-change
		 :channel-index (validate-channel-index channel-index)
		 :data1 (validate-data program-number)
		 :data2 0))

(defmethod data-count ((evn program-change)) 1)

