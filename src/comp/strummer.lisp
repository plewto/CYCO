;;;; cyco.comp.strummer
;;;;
;;;; STRUMMER is a type of PART with many options for "strumming" chords.
;;;; STRUMMER uses the GTRCHORD function (instead of the general chord
;;;; assignments) which restricts chord structures to those possible
;;;; on a standard tenor guitar. 
;;;; 

(in-package :cyco)

(defclass strummer (part)
  ((delay-scale
    :type function
    :initform (constantly 1.0))
   (amplitude-scale
    :type function
    :initform (constantly 1.0))
   (duration-scale
    :type function
    :initform (constantly 1.0)) ))


(defmacro strummer (name instrument &key     ;; only single instrument
			 (period nil)
			 (qfn #'bar)
			 ;;(amp-map #'identity)
			 ;;(duration-map #'identity)
			 (project *project*)
			 (section nil)
			 (events '()))
  "Creates instance of STRUMMER class.  The part is bound to the symbol name
   and is automatically linked as a child node for the current section in the 
   current project.

   ARGS:
     name       - (unquoted) symbol
     instrument - Instrument.  Unlike most other subclasses of PART, STRUMMER
                  uses a single instrument only.
     :period    - Duration of part in seconds, defaults to duration of
                  parent section
     :qfn       - Function used to parse time values, defaults to #'BAR
     :project   - Default *PROJECT*
     :section   - Defaults to current section of project.  If specified
                  section *MUST* be a child node of project.
     :events    - List of event specifications. See below.

   As with EPART, events are specified as a nested list
   
       ((event-1)
        (event-2)
         ......
        (event-n))
   
   Each event may have any of the following clauses. Unspecified clauses 
   usually default to the previous value.

   :time         - Event time specification as expected by cuing function QFN.
                   For the default #'BAR cuing function a time clause has the 
                   form  :time (bar beat subbeat)  or 
                         :time (bar beat sub tsub tick)
   :key          - Keynumber, the keynumber is not used in the typical 
                   manner. The pitch-class selects the chord root, while 
                   the octave number selects a neck position.  Generally
                   higher octaves are played higher up the neck.  Not all 
                   combinations of pitch-class and chord-type have the same 
                   number of possible variations.  If the octave number
                   exceeds the available variations, the highest possible 
                   chord is produced.
   :chord        - Keyword, chord type selection.  +GTR-CHORD-FAMILIES+
                   contains a list of possible values.
                   :SOLO 
                   :MAJ :MAJ7 :DOM7 :MAJ9 :MAJ11 
                   :MIN :MIN7 :MIN9 :MIN11 :DIM
                   :6TH :MIN6 :SUS4 :SUS2
   :strum        - Sets delay between individual chord notes.  The delay may 
                   either be in seconds or as a metric symbol.   
                   
                   :strum delay

                   An alternate form allows progressive changes in the delay 
                   time.

                   :strum (delay scale)

                   Where scale is an accelration factor.

                       note 0  <-  time
                       note 1  <-  time + (delay * scale^0)
                       note 2  <-  time + (delay * scale^1)
                       note 3  <-  time + (delay * scale^2)
   :dur          - Note duration either as a float or a metric symbol.
                  
                   :dur d

                  An alternate form allows random variation for duration of 
                  each note.

                  :dur (d variance)

                  Where variance is the maximum ratio between specified 
                  duration and actual value.   See APPROX function.
                  0.0 <= variance <= 1.0
   :end-together - Flag indicating if all notes should end at the same time, 
                   or be staggered with note start-time.  The end-time flag 
                   does NOT override duration variation.  
   :amp          - Note amplitude, either as float or symbol. 

                   :amp a

                   An alternate form allows progressive changes to amplitude
                   values and random variations.

                   :amp (a scale variance)
   :direction    - Selects direction in which notes are strummed.  Possible 
                   values are 
                       :up
                       :down
                       :coin (select up or down randomly)
                       :random (select notes in random order)
                  
                   :direction :up

                   Unlike the strum, dur and amp values, which are applied 
                   to individual notes in a chord, direction is applied to 
                   a sequence of chords.  An alternate form allows for a 
                   cycle of chord directions

                   :direction (:up :up :down)

                   The first two chords are played up and every third chord is
                   played down.  The cycle then repeats.
   :cc           - Produces a single MIDI control change event

                   :cc ctrl :value v

                   Where ctrl is a controller number or assigned controller 
                   alias and value is a float 0.0 <- value <= 1.0
   :bend         - Produces single MIDI pitch bend event

                   :bend amount

                   Where -1.0 <= amount +1.0
   :program      - Executes instrument program-change-hook
                   
                  :program number
                    
                  or

                  :program number :bank b"
  `(progn
     (validate-part-parents ,project ,section)
     (let* ((sec (or ,section (current-section ,project)))
	    (prt (make-instance 
		  'strummer
		  :name ',name
		  :instruments (->instrument-list ,instrument ,project)
		  :period (or ,period (duration sec)))))
       (add-child! sec prt)
       (property! prt :transposable t)
       (property! prt :qfn ,qfn)
       ;;(property! prt :amplitude-map ,amp-map)
       ;;(property! prt :duration-map ,duration-map)
       (part-banner sec prt)
       (param ,name prt)
       (setf (events prt) ,events)
       prt)))

(defmethod render-once ((prt strummer) &key (offset 0.0))
  (let* ((instrument (car (instruments prt)))
	 (channel-index (1- (channel instrument)))
	 (cc-map (property instrument :controller-assignments))
	 (program-hook (property instrument :program-change-hook))
	 (qfn (property prt :qfn))
	 (time offset)
	 (keynumber 0)
	 (chord :solo)
	 (delay 0.01)
	 (delay-scale 1.0)
	 (direction-pattern (cycle :of '(:down)))
	 (duration 'q)
	 (duration-variance 0.0)
	 (end-together nil)
	 (amplitude 'mf)
	 (amplitude-scale 1.0)
	 (amplitude-variance 0.0)
	 (msg1 "~A is not valid strummer chord (strummer ~A)")
	 (msg2 "~A is not valid strummer chord direction.")
	 (acc '()))
    (flet ((parse-time (evn)
		       ;; :time (x x x)
		       (let ((spec (cdr (assoc :time evn))))
		       	 (set-if time (+ offset (apply qfn spec)))))
	   (parse-keynum (evn)
			 ;; :key kn
			 (let ((spec (cdr (assoc :key evn))))
			   (set-if keynumber (keynumber spec))))
	   (parse-chord (evn)
			;; :chord type
			(let ((spec (cdr (assoc :chord evn))))
			  (if spec
			      (if (member spec +GTR-CHORD-FAMILIES+)
				  (setf chord spec)
				(error (format nil msg1 spec (name prt)))))
			  chord))
	   (parse-delay (evn)
			;; :strum delay
			;; :strum (delay scale)
			(let ((spec (cdr (assoc :strum evn))))
			  (if spec
			      (progn 
				(setf spec (fill-list (->list spec)
						      (list (metric delay)
							    delay-scale)))
				(setf delay (metric (car spec)))
				(setf delay-scale (float (second spec)))))
			  (list delay delay-scale)))
	   (parse-direction (evn)
			    ;; :direction d
			    ;; :direction (d1 d2 d3 ...)
			    (let ((spec (cdr (assoc :direction evn))))
			      (if spec
				  (setf direction-pattern
					(cycle :of (->list spec))))
			      direction-pattern))
	   (parse-duration (evn)
			   ;; :dur d
			   ;; :dur (d variance)
			   (let ((spec (cdr (assoc :dur evn))))
			     (if spec
				 (progn
				   (setf spec (fill-list 
					       (->list spec)
					       (list duration
						     duration-variance)))
				   ;;(setf duration (metric (car spec)))
				   (setf duration (parse-metric-expression (car spec)))
				   (setf duration-variance 
					 (float (second spec)))))
			     (list duration duration-variance)))
	   (parse-end-together (evn)
			       ;; :end-together flag
			      (let ((spec (assoc :end-together evn)))
				(if spec
				    (setf end-together (cdr spec)))
				end-together))
	   (parse-amplitude (evn)
			    ;; :amp a
			    ;; :amp (a scale variance)
			    (let ((spec (cdr (assoc :amp evn))))
			      (if spec
				  (progn 
				    (setf spec (fill-list 
						(->list spec)
						(list amplitude
						      amplitude-scale
						      amplitude-variance)))
				    (setf amplitude (amplitude (car spec)))
				    (setf amplitude-scale 
					  (float (second spec)))
				    (setf amplitude-variance 
					  (float (third spec)))))
			      (list amplitude 
				    amplitude-scale 
				    amplitude-variance)))
	   (handle-cc (evn)
		     ;; :cc ctrl :value v
		     (let ((cc (cdr (assoc :cc evn))))
		       (if cc
			   (let* ((norm-val (or (cdr (assoc :value evn)) 0.0))
				  (value (limit (truncate (* 127 norm-val)) 
						0 127))
				  (ctrl (controller-assignment cc 
							       :cmap cc-map)))
			     (push (cons time (midi-control-change 
					       channel-index ctrl value))
				   acc)))))
	   (handle-bend (evn)
			;; :bend value
			(let ((norm (cdr (assoc :bend evn))))
			  (if norm
			      (let* ((bytes (bend->midi-data norm))
				     (lsb (aref bytes 0))
				     (msb (aref bytes 1)))
				(push (cons time (midi-pitch-bend 
						  channel-index lsb msb))
				      acc)))))
	   (handle-program (evn)
			   ;; :program p
			   ;; :program p :bank b
			   (let ((p (cdr (assoc :program evn)))
				 (b (cdr (assoc :bank evn))))
			     (if p
				 (setf acc (append acc (funcall program-hook
								(- time 0.01)
								channel-index
								p b))))))
	   (handle-chord (evn)
			 (if (or (member :key evn :test #'(lambda (a b)
							    (eq a (car b))))
				 (member :chord evn :test #'(lambda (a b)
							      (eq a (car b)))))
			     (let* ((chord-keylist (gtrchord keynumber chord))
				    (dir (next-1 direction-pattern))
				    (keylist (cond ((eq dir :up)
						    (reverse chord-keylist))
						    ((eq dir :down)
						     chord-keylist)
						    ((eq dir :coin)
						     (coin 0.5 chord-keylist 
							   (reverse chord-keylist)))
						    ((eq dir :random)
						     (permutation chord-keylist))
						    (t
						     (cyco-warning
						      (format nil msg2 dir))
						     chord-keylist)))
				    (start-time time)
				    (current-time start-time)
				    (current-delay delay)
				    (current-amp (amplitude amplitude))
				    )
			       (dolist (kn (keynumber keylist))
				 (let* ((dur (approx (float (metric duration)) 
						     :scale duration-variance))
					(end-time (if end-together
						      (+ start-time dur)
						    (+ current-time dur)))
					(amp (instrument-amplitude
					      instrument current-amp))
					(vel (limit (truncate (* amp 127)) 1 127)))
				   (push (cons current-time 
					       (midi-note-on 
						channel-index kn vel))
					 acc)
				   (push (cons end-time 
					       (midi-note-off 
						channel-index kn 0)) 
					 acc)
				   (setf current-time 
					 (+ current-time current-delay))
				   (setf current-delay 
					 (* current-delay delay-scale))
				   (setf current-amp 
					 (approx (* current-amp amplitude-scale) 
						 :scale amplitude-variance))))))) )
      (if (not (mute? prt))
	  (dolist (evn (events prt))
	    (let ((alst (->alist evn)))
	      (parse-time alst)
	      (parse-keynum alst)
	      (parse-chord alst)
	      (parse-delay alst)
	      (parse-direction alst)
	      (parse-duration alst)
	      (parse-end-together alst)
	      (parse-amplitude alst)
	      (handle-cc alst)
	      (handle-bend alst)
	      (handle-program alst)
	      (handle-chord alst) )))
      acc))) 
