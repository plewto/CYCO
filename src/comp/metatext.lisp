;;;; cyco comp.metatext
;;;;
;;;; A MARKER is a PART which wraps a single MIDI meta marker event.
;;;;

(in-package :cyco)

(defclass marker (part)
  ((qfn
    :type function
    :accessor qfn
    :initform #'bar
    :initarg :qfn)
   (time
    :type t
    :accessor marker-time
    :initform '(1 1 1)
    :initarg :time)
   (text
    :type t
    :accessor marker-text
    :initform "MARKER"
    :initarg :text)))

(defun marker (text &key
		    (qfn #'bar)
		    (time '(1 1 1))
		    (period nil)
		    (project *project*)
		    (section nil))
  (validate-part-parents project section)
  (let* ((sec (or section (current-section project)))
	 (prt (make-instance 'marker
			     :time time
			     :name (format nil "MARKER-~A" text)
			     :period (or period (duration sec))
			     :text (->string text))))
    (add-child! sec prt)
    (property! prt :qfn qfn)
    prt))

(defmethod render-once ((prt marker) &key (offset 0))
  (if (not (mute? prt))
      (let ((time (+ offset (apply (property prt :qfn)
				   (marker-time prt))))
	    (text (marker-text prt)))
	(list (cons time (marker-event text))))
    nil))
			       
  
    
		  
   
