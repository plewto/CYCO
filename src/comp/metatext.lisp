;;;; cyco comp.metatext
;;;;
;;;; A MARKER is a PART which wraps a single MIDI meta marker event.
;;;;

(in-package :cyco)

(defclass text-part (part)
  ((meta-type
    :type keyword
    :accessor meta-type
    :initform :text  ;; :text :copyright :lyric :marker :cue
    :initarg :meta-type)
   (time
    :type t
    :accessor event-time
    :initform '(1 1 1)
    :initarg :time)
   (text
    :type string
    :accessor text
    :initform ""
    :initarg :text)))

;; (defmethod render-once ((prt text-part) &key (offset 0))
;;   (if (not (mute? prt))
;;       (let ((time (+ offset (apply (property prt :qfn)
;; 				   (event-time prt))))
;; 	    (text (text prt))
;; 	    (mtype (meta-type prt))
;; 	    (proj (parent (parent prt))))
;; 	(setf (current-section proj)(parent prt))
;; 	(cond ((eq mtype :text)
;; 	       (list (cons time (text-event text))))
;; 	      ((eq mtype :copyright)
;; 	       (list (cons time (copyright-event text))))
;; 	      ((eq mtype :lyric)
;; 	       (list (cons time (lyric-event text)))
;; 	      ((eq mtype :marker)
;; 	       (list (cons time (marker-event text))))
;; 	      ((eq mtype :cue)
;; 	       (list (cons time (cue-event text))))
;; 	      (t
;; 	       (let ((msg (format nil "~A is not a valid META text type" mtype)))
;; 		 (cyco-warning msg)
;; 		 nil)))))
;; 	nil))


(defmethod render-once ((prt text-part) &key (offset 0))
  (if (not (mute? prt))
      (let ((time (+ offset (apply (property prt :qfn)
				   (event-time prt))))
	    (text (text prt))
	    (mtype (meta-type prt))
	    (proj (parent (parent prt)))
	    (rs nil))
	(setf (current-section proj)(parent prt))
	(cond ((eq mtype +TEXT-EVENT+)
	       (setf rs (list (cons time (text-event text)))))
	      ((eq mtype +COPYRIGHT+)
	       (setf rs (list (cons time (copyright-event text)))))
	      ((eq mtype +LYRIC-TEXT+)
	       (setf rs (list (cons time (lyric-event text)))))
	      ((eq mtype +MARKER-TEXT+)
	       (setf rs (list (cons time (marker-event text)))))
	      ((eq mtype +CUE-POINT+)
	       (setf rs (list (cons time (cue-event text)))))
	      (t
	       (let ((msg (format nil "~A is not a valid META text type" mtype)))
		 (cyco-warning msg)
		 nil))
	      rs))
    nil))

(flet ((--text-event (text &key
			   (type :text)
			   (qfn #'bar)
			   (time '(1 1 1))
			   (period nil)
			   (project *project*)
			   (section nil))
		     (validate-part-parents project section)
		     (let* ((mtype (cond ((eq type :text) +TEXT-EVENT+)
					 ((eq type :copyright) +COPYRIGHT+)
					 ((eq type :lyric) +LYRIC-TEXT+)
					 ((eq type :marker) +MARKER-TEXT+)
					 ((eq type :cue) +CUE-POINT+)
					 (t +TEXT-EVENT+)))
			    (sec (or section (current-section project)))
			    (prt (make-instance 'text-part
						:meta-type mtype
						:time time
						:name (format nil "META ~A" type)
						:period (or period (duration sec))
						:text (->string text))))
		       (add-child! sec prt)
		       (property! prt :qfn qfn)
		       prt)))
  (defun marker (text &key
		      (qfn #'bar)
		      (time '(1 1 1))
		      (period nil)
		      (project *project*)
		      (section nil))
    (--text-event text
		  :type :marker
		  :qfn qfn
		  :time time
		  :period period
		  :project project
		  :section section))
  (defun copyright (text &key
			 (qfn #'bar)
			 (time '(1 1 1))
			 (period nil)
			 (project *project*)
			 (section nil))
    (--text-event text
		  :type :copyright
		  :qfn qfn
		  :time time
		  :period period
		  :project project
		  :section section))
  (defun lyric (text &key
		     (qfn #'bar)
		     (time '(1 1 1))
		     (period nil)
		     (project *project*)
		     (section nil))
    (--text-event text
		  :type :lyric
		  :qfn qfn
		  :time time
		  :period period
		  :project project
		  :section section))
  
  (defun cue (text &key
		   (qfn #'bar)
		   (time '(1 1 1))
		   (period nil)
		   (project *project*)
		   (section nil))
    (--text-event text
		  :type :cue
		  :qfn qfn
		  :time time
		  :period period
		  :project project
		  :section section)) )
