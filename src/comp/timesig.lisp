;;;; cyco.comp.timesig
;;;;
;;;; The TIMESIG class extends NODE and is a generalization of the common 
;;;; notion of a time signature.
;;;;
;;;; TIMESIG defines a hierarchical nesting of timed events.  At the top level
;;;; is the "phrase", which is composed of a number of bars.  Each bar
;;;; consist of a number of beats, and each beat is divided into subbeats and
;;;; "tsubbeats". A beat is also divided by a number of "ticks".  TIMESIG
;;;; also maintains a tempo. 
;;;;
;;;; Each PROJECT receives a copy of the global *ROOT-TIMESIG* which defines
;;;; the default time-structure and tempo for the project.  SECTIONS within
;;;; the project are free to define their own local time-signature and tempo.
;;;;

(in-package :cyco)

(defgeneric unit (ts))
(defgeneric bar-count (ts))
(defgeneric beat-count (ts))
(defgeneric subbeat-count (ts))
(defgeneric tsubbeat-count (ts))
(defgeneric tick-count (ts))
(defgeneric rep-timesig (ts))

(defclass timesig (node) nil)

(let ((instance-counter 0))
  (flet ((gen-name (arg)
		   (cond ((stringp arg)
			  (intern arg))
			 ((not arg)
			  (let ((name (format nil ":TIMESIG-~A" instance-counter)))
			    (intern name)))
			 ((symbolp arg) 
			  arg))))
    (defun timesig (&key (parent *root-timesig*) name
			 tempo unit bars beats subbeats tsubbeats)
      "Creates new TIMESIG instance.  All non-defined arguments inherit values
       from the parent TIMESIG.

       :parent - The parent TIMESIG, default *ROOT-TIMESIG*
       :name - time signature name, if not supplied a name is produced
               automatically.
       :tempo - tempo in BPM, defaults to parent tempo.
       :unit - symbol, the beat unit. Must be one of 'W 'H 'Q 'E or 'S.
               defaults to parent unit (q).
       :bars - fixnum, number of bars per phrase, defaults to parent value.
       :beats - fixnum, number of beat units per bar, defaults to parent
                value (4).
       :subbeats - number of subbeats per beat.  The typical value of 4
                indicates 4 sixteenth notes per beat.
       :tsubbeats - number of alternate subbeats per beat.  tsubbeat is
                typically used to define the number of sixteenth note
                triplets per beat (6)."
      (let ((ts (make-instance 'timesig
			       :parent parent
			       :name (gen-name name))))
	(add-child! parent ts)
	(flet ((set?! (key value)

		      (if value
			  (property! ts key value))))
	  
	  (set?! :tempo tempo)
	  (set?! :unit unit)
	  (set?! :bars bars)
	  (set?! :beats beats)
	  (set?! :subbeats subbeats)
	  (set?! :tsubbeats tsubbeats)
	  (setf instance-counter (1+ instance-counter))
	  (if tempo (tempo! ts tempo))
	  ts)))))

(defmethod unit ((ts timesig))
  (property ts :unit))

(defmethod bar-count ((ts timesig))
  (property ts :bars))

(defmethod beat-count ((ts timesig))
  (property ts :beats))

(defmethod subbeat-count ((ts timesig))
  (property ts :subbeats))

(defmethod tsubbeat-count ((ts timesig))
  (property ts :tsubbeats))

(defmethod tick-count ((ts timesig))
  (property ts :count-ticks))

(defmethod tempo ((ts timesig))
  "Returns TIMESIG tempo in BPM."
  (property ts :tempo))

(defmethod tempo! ((ts timesig)(bpm number))
  "Update TIMESIG tempo."
  (let* ((u (unit ts)))
    (let* ((dur-beat (/ (* 60.0 (metric u)) bpm))
	   (dur-bar (* (beat-count ts) dur-beat))
	   (dur-phrase (* (bar-count ts) dur-bar))
	   (dur-sub (/ dur-beat (subbeat-count ts)))
	   (dur-tsub (/ dur-beat (tsubbeat-count ts)))
	   (dur-tick (/ dur-beat (tick-count ts))))
      (property! ts :tempo bpm)
      (property! ts :dur-phrase dur-phrase)
      (property! ts :dur-bar dur-bar)
      (property! ts :dur-beat dur-beat)
      (property! ts :dur-subbeat dur-sub)
      (property! ts :dur-tsubbeat dur-tsub)
      (property! ts :dur-tick dur-tick)
      ts)))

(setf *root-timesig*
      (let ((root (make-instance 'timesig
				 :name 'ROOT-TIMESIG
				 :parent nil
				 :transient nil)))
	    (property! root :tempo 60)
	    (property! root :unit 'q)
	    (property! root :bars 1)
	    (property! root :beats 4)
	    (property! root :subbeats 4)
	    (property! root :tsubbeats 6)
	    (property! root :count-ticks +TICKS-PER-QUARTER-NOTE+)
	    root))

(tempo! *root-timesig* 60)
      
(defmethod duration ((ts timesig))
  "Returns duration of the TIMESIG, which is the same as the
   phrase duration."
  (property ts :dur-phrase))

(defmethod phrase-duration ((ts timesig))
  "Returns duration of single TIMESIG phrase."
  (property ts :dur-phrase))

(defmethod bar-duration ((ts timesig))
  (property ts :dur-bar))

(defmethod beat-duration ((ts timesig))
  (property ts :dur-beat))

(defmethod subbeat-duration ((ts timesig))
  (property ts :dur-subbeat))

(defmethod tsubbeat-duration ((ts timesig))
  (property ts :dur-tsubbeat))

(defmethod tick-duration ((ts timesig) &optional (unit :ignore))
  (dismiss unit)
  (property ts :dur-tick))

(defmethod rep-timesig ((ts timesig))
  (let ((br (property ts :bars))
	(bt (property ts :beats))
	(sub (property ts :subbeats))
	(tsub (property ts :tsubbeats))
	(u (property ts :unit))
	(tmp (property ts :tempo)))
    (if (or (/= sub 4)(/= tsub 6))
	(format nil "bars: ~A  ~A/~A  sub[~A ~A] tempo: ~A"
		br bt u sub tsub tmp)
      (format nil "bars: ~A  ~A/~A tempo: ~A"
	      br bt u tmp))))

(defmethod ->string ((ts timesig))
  (str+ "Timesig " (rep-timesig ts)))
