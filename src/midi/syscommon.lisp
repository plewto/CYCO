;;;; cyco.midi.syscommon
;;;;
;;;; Implemented events
;;;;     system-exclusive
;;;;     end-exclusive
;;;;
;;;; un-implemented 
;;;;     midi-time-code
;;;;     song-position-pointer
;;;;     song-select
;;;;     tune-request
;;;;


(in-package :cyco)

(defclass system-common-event (midi-event) nil)
  
(defmethod system-common-event-p ((obj system-common-event)) obj)

;;; ---------------------------------------------------------------------- 
;;;			     SYSTEM-EXCLUSIVE-EVENT


(defclass system-exclusive-event (system-common-event)
  ((command
    :type fixnum
    :reader command
    :initform +SYSTEM-EXCLUSIVE+)
   (priority
    :initform 5)
   (data
    :type vector
    :initform #()
    :initarg :data)))

(defmethod system-exclusive-event-p ((obj system-exclusive-event)) obj)

(defmethod data-count ((syx system-exclusive-event))
  (length (slot-value syx 'data)))

(defmethod data ((syx system-exclusive-event)(index fixnum))
  (aref (slot-value syx 'data) index))

(defmethod data! ((syx system-exclusive-event)(index fixnum)(value fixnum))
  (setf (aref (slot-value syx 'data) index) (logand value #x7F)))

(defmethod ->string ((syx system-exclusive-event))
  (str+ (call-next-method)
	(format nil "count: ~A" (data-count syx))))

(defmethod render-event ((sys system-exclusive-event))
  (cons (command syx)
	(->list (slot-value syx 'data))))

(defmethod clone ((syx system-exclusive-event) &key 
		  newname parent (hook #'identity))
  (let ((other (midi-system-exclusive-event 
		:data (clone (slot-value syx 'data)))))
    (funcall hook other)))

(defun midi-system-exclusive-event (&key data count)
  (if (and data count)
      (let ((msg "Either :data or :count argument must be supplied to system-exclusive-event, but not both"))
	(error msg)))
  (if (not (or data count))
      (let ((msg "Either :data or :count must be supplied to system-exclusive-event"))
	(error msg)))
  (let ((d data))
    (if count
	(progn
	  (setf d (make-array count :element-type 'fixnum))
	  (dotimes (i count)
	    (setf (aref d i) 0))))
    (make-instance 'system-exclusive-event :data d)))

;;; ---------------------------------------------------------------------- 
;;;			END-SYSTEM-EXCLUSIVE-EVENT

(defclass end-system-exclusive-event (system-common-event)
  ((command
    :type fixnum
    :reader command
    :initform +END-EXCLUSIVE+)
   (priority
    :initform 6)))

(defun midi-end-system-exclusive ()
  (make-instance 'end-system-exclusive-event))

(defmethod render-event ((eox end-system-exclusive-event))
  (list +END-EXCLUSIVE+))

(defmethod clone ((eox end-system-exclusive-event) &key 
		  newname parent (hook #'identity))
  (funcall hook (midi-end-system-exclusive)))

