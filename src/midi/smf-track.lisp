;;;; cyco.midi.smf-track
;;;;
;;;; SMF-TRACK defines track chunck for standard MIDI file.
;;;; 

(in-package :cyco)

(defclass smf-track nil
  ((track-name
    :type string
    :reader name
    :initform "Track"
    :initarg :name)
   (initial-tempo
    :type float
    :accessor initial-tempo
    :initform 120
    :initarg :initial-tempo)
   (events				; list of events and event-times
    :type list				; ((time1 . event1)(time2 . event2)...
    :accessor track-events              ; Events should be sorted by time 
    :initform '()			; and then event-priority.
    :initarg :events)))

(defmethod name! ((trk smf-track)(new-name t))
  (setf (slot-value trk 'track-name) (->string new-name)))

(defun --validate-track-event (evn)
  (if (or (and (text-event-p evn)
	       (= (meta-type evn) +TRACK-NAME+))
	  (end-of-track-p evn))
      (let ((msg (format nil "Can not add ~a (~a) to smf-track" (->string evn)(type-of evn))))
	(error msg)))
  evn)

(defmethod push-event ((time number)(evn midi-event)(trk smf-track))
  "Push cons (time . evn) to track events list."
  (--validate-track-event evn)
  (push (cons (float time) evn)
	(track-events trk)))

(defun sort-midi-events-test (a b)
  (let ((ta (car a))
	(tb (car b))
	(pa (priority (cdr a)))
	(pb (priority (cdr b))))
    (if (= ta tb)
	(< pa pb)
      (< ta tb))))

;; (defmethod --sort-smf-track! ((trk smf-track))
;;   (flet ((predicate (a b)
;; 		    (let ((ta (car a))
;; 			  (tb (car b))
;; 			  (pa (priority (cdr a)))
;; 			  (pb (priority (cdr b))))
;; 		      (if (= ta tb)
;; 		      	  (< pa pb)
;; 		      	(< ta tb)))))
;;     (setf (track-events trk)(sort (track-events trk) #'predicate))))

(defmethod --sort-smf-track! ((trk smf-track))
  (setf (track-events trk)(sort (track-events trk) #'sort-midi-events-test)))



(defmethod --init-smf-track-events ((trk smf-track))
  (let ((nevn (track-name-event (name trk)))
	(tevn (tempo-change-event (initial-tempo trk))))
    (list (cons 0 (->list (render-event nevn)))
	  (cons 0 (->list (render-event tevn))))))

(defmethod --render-smf-track ((trk smf-track) &optional (pad-end 1.0))
  (let* ((events (--sort-smf-track! trk))
	 (end-time (+ pad-end (or (car (car (reverse events))) 0)))
	 (beat-unit 'q)
	 (tickdur (tick-duration (initial-tempo trk) beat-unit))
	 (previous-time 0.0)
	 (acc (--init-smf-track-events trk)))
    (setf events (append events
			 (list (cons end-time (end-of-track-event)))))
    (dolist (cevn events)
      (let* ((event-time (car cevn))
	     (event (cdr cevn))
	     (delta-time (prog1
			     (- event-time previous-time)
			   (setf previous-time event-time)))
	     (ticks (truncate (/ delta-time tickdur))))
	(push (fixnum->midi-vlv ticks) acc)
	(push (render-event event) acc)
	;; (if (timesig-event-p event)
	;;     (setf beat-unit (render-event (timesig-unit event))))
	;; (if (tempo-event-p event)
	;;     (setf tickdur (render-event (tick-duration (initial-tempo trk) :unit beat-unit)))) 
	))
    (reverse acc)))
	
(defmethod render-smf-track ((trk smf-track) &optional (pad-end 1))
  (let* ((data1 (--render-smf-track trk pad-end))
	 (data (flatten data1))
	 (bcount (length data))
	 (rs (append (list #x4D #x54 #x72 #x6B
			   (logand (ash bcount -24) #xFF)
			   (logand (ash bcount -16) #xFF)
			   (logand (ash bcount -8) #xFF)
			   (logand bcount #xFF))
		     data)))
    rs))

(defmethod dump-smf-track ((trk smf-track) &key (offset :ignore)(verbose t))
  (dismiss offset)
  (let ((pad (tab 1)))
    (--sort-smf-track! trk) 
    (format t "SMF-TRACK~%")
    (format t "~ANAME        : '~A'~%" pad (name trk))
    (format t "~AINIT TEMPO  : ~A~%" pad (initial-tempo trk))
    (format t "~AEVENT COUNT : ~A~%" pad (length (track-events trk))) 
    (if verbose
	(dolist (pair (track-events trk))
	  (let ((etime (car pair))
		(evn (cdr pair)))
	    (format t "~A~6,4F  : ~A~%" pad etime (->string evn)))))))

(defmethod dump-smf-track ((lst list) &key (offset 0)(verbose t))
  (dump-smf-track (->vector lst) :offset offset :verbose verbose))

(defmethod dump-smf-track ((ary vector) &key (offset 0)(verbose t))
  (let ((pad (tab 1)))
    (flet ((addr (n) (format t "~A[~4X] " pad (+ offset n)))
	   (cr () (format t "~%")))
      (flet ((id ()
		 (addr 0)
		 (format t "ID     : ")
		 (dotimes (i 4)
		   (format t "~2X " (aref ary (+ offset i))))
		 (cr))
	     (trklen ()
		     (addr 4)
		     (format t "LENGTH : ")
		     (let ((acc 0)
			   (scale #xFD02FF)
			   (byte nil))
		       (dotimes (i 4)
			 (setf byte (aref ary (+ offset 4 i)))
			 (setf acc (+ acc (* scale byte)))
			 (setf scale (/ scale 255))
			 (format t "~2X " byte))
		       (format t "--> ~X~%" acc)
		       acc)))
	(id)
	(trklen)
	(format t "~A------~%" pad)
	(if verbose
	    (xdump ary (+ offset 8) :pad pad))))))
