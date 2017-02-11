;;;; cyco.midi.meta
;;;;
;;;; Implemented meta events
;;;;    text-event
;;;;    copyright-notice
;;;;    track-name
;;;;    instrument-name
;;;;    lyric-name
;;;;    marker-event
;;;;    cue-text-event
;;;;    end-of-track
;;;;    tempo-change
;;;;    time-signature
;;;;
;;;; Un-implemented meta events
;;;;    sequence number
;;;;    midi-channel-prefix
;;;;    smpte-offset
;;;;    key-signature
;;;;    sequencer-specific event

(in-package :cyco)

(defclass meta-event (midi-event)
  ((command				;; Always +META+
    :type fixnum
    :reader command
    :initform +META+)
   (priority
    :initform 0)
   (meta-type				;; Meta types defined in constants.
    :type fixnum
    :reader meta-type
    :initform nil
    :initarg :meta-type)))

(defmethod meta-event-p ((obj meta-event)) obj)

(defmethod ->string ((obj meta-event))
  (format nil "META ~9A " (gethash (meta-type obj) +MNEMONICS+)))

;;; ---------------------------------------------------------------------- 
;;;			     Meta text events

(defclass meta-text-event (meta-event)
  ((text
    :type string
    :reader text
    :initform ""
    :initarg :text)
   (priority
    :initform 0)))

(defmethod text-event-p ((obj meta-text-event)) obj)

(defun text-event (txt)
  (make-instance 'meta-text-event
		 :meta-type +text-event+
		 :text (->string txt)))

(defun copyright-event (txt) 
  (make-instance 'meta-text-event
		 :meta-type +copyright+
		 :text (->string txt)))

(defun track-name-event (txt)
  (make-instance 'meta-text-event
		 :meta-type +track-name+
		 :text (->string txt)))

(defun instrument-event (txt)
  (make-instance 'meta-text-event
		 :meta-type +instrument-name+
		 :text (->string txt)))

(defun lyric-event (txt)
  (make-instance 'meta-text-event
		 :meta-type +lyric-text+
		 :text (->string txt)))

(defun marker-event (txt)
  (make-instance 'meta-text-event
		 :meta-type +marker-text+
		 :text (->string txt)))

(defun cue-event (txt)
  (make-instance 'meta-text-event
		 :meta-type +cue-point+
		 :text (->string txt)))

(defmethod clone ((obj meta-text-event) &key 
		  newname parent (hook #'identity))
  (funcall hook (make-instance 'meta-text-event
			       :meta-type (meta-type obj)
			       :text (->string (text obj)))))

(defmethod ->string ((obj meta-text-event))
  (str+
   (call-next-method)
   (format nil "'~A'" (text obj))))

(defmethod text! ((obj meta-text-event)(new-text t))
  (setf (slot-value obj 'text)(->string new-text)))

(defmethod render-event ((obj meta-text-event))
  (let* ((txt (text obj))
	 (count (length txt))
	 (tcc (let ((acc '()))
		(dotimes (i count)
		  (push (char-code (char txt i)) acc))
		(reverse acc))))
    (append (list +META+ (meta-type obj))
	    (fixnum->midi-vlv count)
	    tcc)))
		      
;;; ---------------------------------------------------------------------- 
;;;			       End of Track

(defclass end-of-track-event (meta-event)
  ((meta-type
    :type fixnum
    :reader meta-type
    :initform +END-OF-TRACK+)
   (priority
    :initform 99)))

(defun end-of-track-event ()
  (make-instance 'end-of-track-event))

(defmethod end-of-track-p ((obj end-of-track-event)) obj)

(defmethod clone ((eot end-of-track-event) &key 
		  newname parent (hook #'identity))
  (funcall hook (end-of-track-event)))

(defmethod ->string ((eot end-of-track-event))
  (gethash +END-OF-TRACK+ +MNEMONICS+))

(defmethod render-event ((eot end-of-track-event))
  '(#xFF #x2F 0))


;;; ---------------------------------------------------------------------- 
;;;				Tempo Change

(defclass tempo-change-event (meta-event)
  ((meta-type
    :type fixnum
    :reader meta-type
    :initform +TEMPO-CHANGE+)
   (priority
    :initform 2)
   (tempo
    :type float
    :reader tempo
    :initform 120
    :initarg :bpm)))

(defmethod tempo-event-p ((obj tempo-change-event)) obj)

(defun tempo-change-event (bpm)
  (make-instance 'tempo-change-event :bpm (float bpm)))

(defmethod clone ((evn tempo-change-event) &key 
		  newname parent (hook #'identity))
  (funcall hook (tempo-change-event (tempo evn))))

(defmethod ->string ((evn tempo-change-event))
  (str+ (call-next-method)
	(format nil "BPM: ~A" (tempo evn))))

(defmethod micro-seconds-per-beat ((evn tempo-change-event))
  (bpm->microseconds (tempo evn)))

(defmethod tick-duration ((evn tempo-change-event) &optional (unit :ignore))
  (tick-duration (tempo evn)))

(defmethod render-event ((evn tempo-change-event))
  (let* ((usec (micro-seconds-per-beat evn))
	 (d0 (logand (ash usec -16) #xFF))
	 (d1 (logand (ash usec -8) #xFF))
	 (d2 (logand usec #xFF)))
    (list +META+ +TEMPO-CHANGE+ 3 d0 d1 d2)))
	     

;;; ---------------------------------------------------------------------- 
;;;			      Time Signature

(defclass timesig-event (meta-event)
  ((meta-type
    :type fixnum
    :reader meta-type
    :initform +TIME-SIGNATURE+)
   (priority
    :initform 1)
   (numerator
    :type fixnum
    :accessor timesig-numerator
    :initform 4
    :initarg :num)
   (unit
    :type symbol
    :accessor timesig-unit
    :initform 'q
    :initarg :unit)
   (tpq					; The MIDI specs are vague
    :type fixnum			; on these last two values.
    :initform +TICKS-PER-QUARTER-NOTE+)	; They are treated as constants
   (met					; here.
    :type fixnum
    :initform 8)))

(defmethod timesig-event-p ((obj timesig-event)) obj)

(defun --map-timesig-unit->denominator (sym)
  (cond ((eq sym 'q) 2)
	((eq sym 'w) 0)
	((eq sym 'h) 1)
	((eq sym 'e) 3)
	((eq sym 's) 4)
	(t (error (format nil "Illegal time-sign unit ~A, expecting one of W H Q E or S" sym)))))

(defun --map-timesig-denominator->unit (d)
  (aref #(W H Q E S) d))

(defun timesig-event (num unit)
  (--map-timesig-unit->denominator unit) ;; check validity of unit
  (make-instance 'timesig-event
		 :num num
		 :unit unit))

(defmethod clone ((evn timesig-event) &key 
		  newname parent (hook #'identity))
  (funcall hook (timesig-event (timesig-numerator evn)
			       (timesig-unit evn))))

(defmethod ->string ((evn timesig-event))
  (str+ (call-next-method)
	(format nil "~A/~A"
		(timesig-numerator evn)
		(timesig-unit evn))))

(defmethod render-event ((evn timesig-event))
  (list +META+ +TIME-SIGNATURE+ 4
	  (timesig-numerator evn)
	  (--map-timesig-unit->denominator (timesig-unit evn))
	  (slot-value evn 'tpq)
	  (slot-value evn 'met)))

(defmethod timesig-scale-factor ((evn timesig-event))
  (let ((u (timesig-unit evn)))
    (cond ((eq u 'q) 1.0)
	  ((eq u 'w) 4.0)
	  ((eq u 'h) 2.0)
	  ((eq u 'e) 0.5)
	  ((eq u 's) 0.25)
	  (t 1.0))))
