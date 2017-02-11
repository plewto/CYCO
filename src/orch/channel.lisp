;;;; cyco.orch.channel
;;;;
;;;; Defines CHANNEL-ASSIGNMENT-MAP class to assign symbolic names to MIDI
;;;; channels.  MIDI channels are defined between 1 and 16 inclusive.  The
;;;; term "channel-index" is used for the actual byte value of a MIDI message.
;;;;
;;;; MIDI channel assignments may be nested. That is a symbolic channel name
;;;; may map to another symbolic name. Ultimately though one of the symbols
;;;; must map to an an actual channel number.   It is possible to define 
;;;; circular channel assignments.  The *MAX-CHANNEL-ASSIGNMENT-NEST-DEPTH*
;;;; variable is used to guard against circular assignments.
;;;;
;;;; CHANNEL-ASSIGNMENT-MAP extends NODE and each PROJECT uses
;;;; *GLOBAL-CHANNEL-ASSIGNMENTS* as a default
;;;;

(in-package :cyco)

(defclass channel-assignment-map (node) nil)

(setf *global-channel-assignments*
      (make-instance 'channel-assignment-map
		     :name :global-channel-assignments
		     :parent nil
		     :transient nil))

(defun channel-assignment-map (&key
			       (name :unnamed-channel-assignment-map)
			       (parent *global-channel-assignments*)
			       (transient t))
  (make-instance 'channel-assignment-map
		 :name name
		 :parent parent
		 :transient transient))

(defmethod assign-channel! ((alias t)(value t) &key
			    (cmap *global-channel-assignments*))
  (property! cmap alias value))

(defmethod channel-assignment ((n fixnum) &key
			       (cmap :ignore)
			       (resolve :ignore))
  (dismiss cmap resolve)
  (1+ (logand (1- n) #xF)))

(defmethod --channel-assignment-once ((cmap channel-assignment-map)(alias symbol))
  (or (property cmap alias :default nil) :error))

(defmethod --resolve-channel-assignment ((cmap channel-assignment-map)
					 (alias symbol)
					 (depth fixnum))
  (if (plusp depth)
      (let ((n (--channel-assignment-once cmap alias)))
	(if (integerp n)
	    n
	  (--resolve-channel-assignment cmap n (1- depth))))
    (progn
      (cyco-warning (format nil "MIDI channel assignment depth exceeded   '~A'" alias))
      :error)))

(defmethod channel-assignment ((alias symbol) &key
			       (cmap *global-channel-assignments*)
			       (resolve nil))
  (let ((rs (if (not resolve)
		(--channel-assignment-once cmap alias)
	      (--resolve-channel-assignment cmap alias 12))))
    (if (eq rs :error)
	(error (format nil "Unknown MIDI channel '~A'" alias))
      rs)))

(defmethod channel-assignments (&optional (cmap *global-channel-assignments*))
  (let* ((keys (property-keys cmap))
	 (acc '()))
    (dotimes (i 16)
      (push (cons (1+ i)(1+ i)) acc))
    (dolist (k keys)
      (let ((c (channel-assignment k :cmap cmap :resolve t)))
	(push (cons k c) acc)))
    (setf acc (sort acc #'(lambda (a b)
			    (string< (->string (car a))(->string (car b))))))
    (setf acc (stable-sort acc #'(lambda (a b)
				   (< (cdr a)(cdr b)))))
    acc))
    
