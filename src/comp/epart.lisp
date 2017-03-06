;;;; cyco.comp.epart
;;;; 
;;;; EPART is a part which allows explicit event specifications.
;;;;

(in-package :cyco)

(defclass epart (part) nil)

(defmacro epart (name instruments &key
		      (period nil)
		      (qfn #'bar)
		      (amp-map #'identity)
		      (duration-map #'identity)
		      (project *project*)
		      (section nil)
		      (events '()))
  "Creates an instance of EPART class.  The part is bound to the symbol name
  and is automatically linked as a child node of the current section in the 
  current project.

  ARGS:
     name - (unquoted) symbol.  The new instance is bound to name.
     instruments - instrument or list of instruments.
                   All instruments are layerd.
    :period - Duration of part in seconds, defaults to duration of
              parent section.
    :qfn    - Function used to parse or 'cue' time values.
              Defaults to #'BAR.
    :amp-map - Amplitude map function applied to amplitude values
             prior to the instruments mapping functions.  All unrecognized
             symbols should be returned as the functions result.
             Default #'IDENTITY
    :duration-map - Duration mapping function applied to duration values
             prior to the instruments mapping function  All unrecognized
             symbols should be returned as the functions result.
             Default #'IDENTITY
    :project - The CYCO project this part belongs to, Default *PROJECT*.
    :section - The section to which this part belongs.  Defaults to 
             the current section of project.
    :events - A list of event specifications.

     There are several possible event types and details are dependent 
     on the specific type.  Events are specified as a nested list

         '((event-1
            event-2
            .......
            event-n))

     Each event is specified by a series of key/value pairs and may produce
     multiple MIDI events.  Some event fields are optional.  In most cases
     a missing field defaults to the previous value.  The following 
     list details the various event types.   The time field is common 
     to all event types and must be in a format expected by the cuing function
     qfn.   The following assume qfn is #'BAR and time specifications
     are :TIME (bar-number beat-number subbeat-number).

     PROGRAM events

        (:TIME (x x x) :PROGRAM p :BANK b)

        Passes program p and bank b to each instrument's program-change-hook.
        :PROGRAM p is mandatory.
        :TIME and :bank are optional.
         
        nil values for either p or bank use instrument's property values 
        for :PROGRAM-NUMBER and :PROGRAM-BANK.  If the instrument does not 
        define these values no event is generated for that instrument.

     CHANNEL PRESSURE EVENTS

        (:TIME (x x x) :TO (y y y) :PRESSURE :START s :END e :STEPS stp)

        Generates channel pressure events between times :TIME and :TO.
        :TIME - optional initial time, defaults to previous event time
        :TO - optional ending time, defaults to one whole note after time.
        :PRESSURE - mandatory 
        :START - optional initial value 0 <= start <=1, default 0
        :END - optional ending value 0 <= end <= 1, default 1
        :STEPS - optional number of events, defaults to previous event
                 steps is initially 100.

      PITCH BEND EVENTS

        (:TIME (x x x) :TO (y y y) :BEND :START s :END e :STEPS stp)

        Generates bend events between times time and to.
        :TIME - optional initial time, defaults to previous event time
        :TO - optional ending time, defaults to one whole note after time.
        :BEND - mandatory 
        :START - optional initial value -1 <= start <=1, default -1
        :END - optional ending value -1 <= end <= 1, default 1
        :STEPS - optional number of events, defaults to previous event
                 steps is initially 100.
        
      CONTROLLER EVENTS

         (:TIME (x x x) :TO (y y y) :CC ctrl :START s :END e :STEPS stp)

        Generates control change events between times time and to.
        :TIME - optional initial time, defaults to previous event time.
        :TO - optional ending time, defaults to one whole note after time.
        :CC ctrl - mandatory controller number.  May be symbolic.
        :START - optional initial value 0 <= start <=1, default 0
        :END - optional ending value 0 <= end <= 1, default 1
        :STEPS - optional number of events, defaults to previous event
                 steps is initially 100.

      KEY EVENTS

         (:TIME (x x x) :KEY kn :DUR d :AMP a)

         Generates single note on/off event pair.
         :TIME - optional time, defaults to previous event time.
         :KEY kn - mandatory keynumber, may be symbolic.
         :DUR d - optional note duration (see metric)
         :AMP a - optional note amplitude.

      CHORD EVENTS
 
         (:TIME (x x x) :KEY kn :CHORD template :DUR d :AMP a :STRUM strm :INVERSION inv)

         Generates chords and arpeggios.
         :TIME  - optional time, defaults to previous event time.
         :KEY   - optional root key, defaults to previous key.
         :CHORD - mandatory, either a chord name from the project's 
                  chord-dictionary or a list of keynumber offsets.
                  For example a major triad is specified either by the symbol
                  [MAJ] or the list (0 4 7). A series of repeated notes may 
                  be created by settintg all in the list to the same value.
         :DUR   - optional note duration, defaults to previous value.
         :AMP   - optional amplitude, defaults to previous value.
         :STRUM - optional :strum rate    or  :strum (rate (pattern...))
                  Where rate is float or metric symbol and pattern 
                  is an arbitrary number of symbols which specifies
                  the direction the chord is played.  possible values are
                    :up      - play backwards
                    :down    - play forward
                    :random  - pick :up or :down at random
                    :permute - use random permutation of note order.
                  Defaults to previous value, initially (0.01 :up :down).
         :INVERSION - optional chord modification (inv octave [copies ...])
                    :inv    - int, chord inversion.
                    :octave - flag, adds octave-copy of first note 
                              (after inversion) to end of note list.
                    :copies - For each number n in copies append a copy of the
                              initial chord transposed by n-steps to end of 
                              chord.
                   Defaults to previous inversion value.

  Returns EPART"
  `(progn
     (validate-part-parents ,project ,section)
     (let* ((sec (or ,section (current-section ,project)))
	    (prt (make-instance 'epart
				:name ',name
				:instruments (->instrument-list ,instruments ,project)
				:period (or ,period (duration sec)))))
       (add-child! sec prt)
       (property! prt :transposable t)
       (property! prt :qfn ,qfn)
       (property! prt :amplitude-map ,amp-map)
       (property! prt :duration-map ,duration-map)
       (part-banner sec prt)
       (param ,name prt)
       (setf (events prt) ,events)
       prt)))

(defmethod dump ((prt epart) &key (depth 0)(max-depth 10))
  (if (< depth max-depth)
      (if (mute? prt)
	  (format t "~AMUTED ~A ~A~%" (tab depth)(type-of prt)(name prt))
	(let ((pad (tab (1+ depth))))
	  (format t "~A~A ~A~%" (tab depth)(type-of prt)(name prt))
	  (if (< (1+ depth) max-depth)
	      (dolist (evn (events prt))
		(format t "~A~A~%" pad evn))))))
  nil)

(let ((default-time 0)
      (default-keynumber 60)
      (default-chord-inversion '(0 nil nil))
      (default-duration 1.0)
      (default-amplitude 0.75)
      (default-strum-rate 0.01)
      (default-strum-pattern (cycle :of '(:up :down)))  ;; :up :down :random
      (default-controller-number 1)
      (default-step-count 100)
      (msg1 "Expected even number of elements in epart events list!"))

  (flet ((->alist (lst)
	 	  (let ((bcc '())
	 		(i 0))
	 	    (if (not (evenp (length lst)))
			(error msg1))
	 	    (while (< i (length lst))
	 	      (push (cons (nth i lst)(nth (1+ i) lst)) bcc)
	 	      (setf i (+ i 2)))
	 	    bcc))

	 (reset-params ()
	 	       (setf default-time 0.0
	 		     default-keynumber 60
	 		     default-chord-inversion '(0 nil nil)
	 		     default-duration 1.0
	 		     default-amplitude 0.75
	 		     default-strum-rate 0.01
			     default-strum-pattern (cycle :of '(:down))
			     default-step-count 100))

	 (parse-time (prt evn)
	 	     (let* ((qfn (property prt :qfn))
	 		    (time-spec (cdr (assoc :time evn)))
	 		    (time (or (and time-spec
	 				   (apply qfn time-spec))
	 			      default-time)))
	 	       (setf default-time time)
	 	       time))

	 (parse-end-time (prt evn)
			 (let* ((qfn (property prt :qfn))
				(time-spec (cdr (assoc :to evn)))
				(time (or (and time-spec 
					       (apply qfn time-spec))
					  (+ default-time (metric 'w)))))
			   time))

	 (parse-start-value (evn)
			    (float (or (cdr (assoc :start evn)) 0)))

	 (parse-end-value (evn)
			  (float (or (cdr (assoc :end evn)) 1)))

	 (parse-step-count (evn)
			   (let ((stp (cdr (assoc :steps evn))))
			     (if stp
				 (setf default-step-count stp))
			     (float default-step-count)))
	 
	 (parse-cc-ctrl (prt evn)
			(let* ((cmap (property prt :controller-assignments))
			       (alias (cdr (assoc :ctrl evn))))
			  (if alias
			      (setf default-controller-number
				    (controller-assignment alias :cmap cmap)))
			  default-controller-number))
	 
	 (parse-keynumber (evn)
	 		  (let* ((kspec (cdr (assoc :key evn)))
	 			 (kn (or (and kspec (keynumber kspec))
	 				 default-keynumber)))
	 		    (setf default-keynumber kn)
	 		    kn))

	 ;; chord inversion spec  (inversion octave-flag copies)
	 (parse-chord-inversion (evn)
	 			(let ((inv (cdr (assoc :inversion evn))))
	 			  (if inv
	 			      (setf default-chord-inversion 
	 				    (fill-list inv '(0 nil nil))))
	 			  default-chord-inversion))

	 ;; chord strum spec (rate (pattern....))
	 ;;  rate - float time between note events in seconds
	 ;;  pattern - instance of cycle
	 ;;              possible values :up :down :random :permute
	 ;;              :up      - use notes in reverse order
	 ;;              :down    - use notes in order
	 ;;              :random  - pick up or down at random
	 ;;              :permute - permute note list
	 ;; 
	 ;; return cons (rate . pattern)
	 ;;
	 (parse-chord-strum (evn)
			    (let ((spec (cdr (assoc :strum evn))))
			      (if spec
				  (let* ((cspec (fill-list (->list spec) '(0.01 (:down))))
					 (rate (metric (car cspec)))
					 (pat (cycle :of (reverse (car (cdr cspec))))))
				    (setf default-strum-rate rate
					  default-strum-pattern pat)))
			      (cons default-strum-rate
				    default-strum-pattern)))
	 (parse-duration (prt evn)
	 		 (let ((dspec (cdr (assoc :dur evn))))
	 		   (if dspec
	 		       (let* ((map (property prt :duration-map))
	 			      (dur (funcall map (parse-metric-expression dspec))))
	 			 (setf default-duration dur)))
	 		   default-duration))

	 (parse-amplitude (prt evn)
	 		 (let ((aspec (cdr (assoc :amp evn))))
	 		   (if aspec
	 		       (let* ((map (property prt :amplitude-map))
	 			      (amp (funcall map (amplitude aspec))))
	 			 (setf default-amplitude amp)))
	 		   default-amplitude)) )
	 
    (flet ((chord-handler (prt evn offset)
    	   		  (setf evn (->alist evn))
    			  (parse-chord-strum evn)
    	   		  (let* ((time (+ offset (parse-time prt evn)))
    	   			 (root (parse-keynumber evn))
    	   			 (inv (parse-chord-inversion evn))
    	   			 (chord-type (cdr (assoc :chord evn)))
    	   			 (note-list (parse-chord 
    					     root chord-type
    					     :inversion (first inv)
    					     :octave (second inv)
    					     :copies (third inv)))
    	   			 (strum-rate default-strum-rate)
    	   			 (strum-pattern default-strum-pattern)
    	   			 (strum-direction (next strum-pattern))
    	   			 (orc (property prt :orchestra))
    				 (dscale (beat-duration (parent prt)))
    	   			 (acc '()))
    	   		    (setf note-list (cond 
    					     ((eq strum-direction :up)
    					      (reverse note-list))
    					     ((eq strum-direction :random)
    					      (if (> (random 100) 50)
    						  (reverse note-list)
    						note-list))
    					     ((eq strum-direction :permute)
    					      (permutation note-list))
    					     (t note-list)))
    			    (dolist (kn note-list)
    			      (dolist (inst-name (->list (instruments prt)))
    				(let* ((inst (find-node orc inst-name))
    				       (cindex (1- (channel inst :resolve t)))
    				       (keynum (instrument-keynumber inst kn))
    				       (dur (* dscale (instrument-duration 
    						       inst (parse-duration prt evn))))
    				       (amp (instrument-amplitude 
    					     inst (parse-amplitude prt evn)))
    				       (t2 (+ time dur)))
    				  (push (cons time (midi-note-on
    						    cindex 
    						    keynum 
    						    (truncate (* 127 amp))))
    					acc)
    				  (push (cons t2 
    					      (midi-note-off cindex keynum 0)) 
    					acc)))
    			      (setf time (+ time strum-rate)))
    			    (reverse acc)))

	   ;; :time :key :dur :amp
	   (note-handler (prt evn offset)
	   		 (setf evn (->alist evn))
	   		 (let* ((time (+ offset (parse-time prt evn)))
	   			(kn (parse-keynumber evn))
	   			(orc (property prt :orchestra))
	   			(dscale (beat-duration (parent prt)))
	   			(acc '()))
	   		   (dolist (inst-name (->list (instruments prt)))
	   		     (let* ((inst (find-node orc inst-name))
	   			    (cindex (1- (channel inst :resolve t)))
	   			    (keynum (instrument-keynumber inst kn))
	   			    (dur (* dscale (instrument-duration
	   					    inst 
	   					    (parse-duration prt evn))))
	   			    (amp (instrument-amplitude 
	   				  inst 
	   				  (parse-amplitude prt evn)))
	   			    (t2 (+ time dur)))
			       (if (and (>= keynum 0)(>= dur 0)(plusp amp))
				   (progn 
				     (push (cons time (midi-note-on 
						       cindex 
						       keynum 
						       (truncate (* 127 amp)))) acc)
				     (push (cons t2 (midi-note-off cindex keynum 0)) 
					   acc)))))
	   		   (reverse acc)))

	   ;; :time :to :ctrl :start :end :steps
	   (cc-handler (prt evn offset)
		       (setf evn (->alist evn))
		       (let* ((orc (property prt :orchestra))
			      (steps (parse-step-count evn))
			      (time (+ offset (parse-time prt evn)))
			      (time2 (+ offset (parse-end-time prt evn)))
			      (delta-time (/ (- time2 time) steps))
			      (ctrl (parse-cc-ctrl prt evn))
			      (start (parse-start-value evn))
			      (end (parse-end-value evn))
			      (delta-value (/ (- end start) steps))
			      (acc '())
			      (ci-list (let ((bcc '()))
			      		 (dolist (inst-name 
						  (->list (instruments prt)))
			      		   (let* ((inst (find-node orc inst-name))
			      			  (ci (1- (channel inst :resolve t))))
			      		     (push ci bcc)))
			      		 bcc))
			      (value start))
			 (while (< time (+ time2 delta-time))
			   (let ((v (truncate (limit (* 127 value) 0 127))))
			     (dolist (ci ci-list)
			       (push (cons time (midi-control-change ci ctrl v)) acc))
			     (setf time (+ time delta-time))
			     (setf value (+ value delta-value))))
			 (reverse acc)))

	   (pressure-handler (prt evn offset)
	   		     (setf evn (->alist (remove :pressure evn)))
	   		     (let* ((orc (property prt :orchestra))
	   			    (steps (parse-step-count evn))
	   			    (time (+ offset (parse-time prt evn)))
	   			    (time2 (+ offset (parse-end-time prt evn)))
				    (delta-time (/ (- time2 time) steps))
				    (start (parse-start-value evn))
				    (end (parse-end-value evn))
				    (delta-value (/ (- end start) steps))
				    (acc '())
				    (ci-list (let ((bcc '()))
					       (dolist (inst-name (->list (instruments prt)))
						 (let* ((inst (find-node orc inst-name))
							(ci (1- (channel inst :resolve t))))
						   (push ci bcc)))
					       bcc))
				    (value start))
			       (while (< time (+ time2 delta-time))
				 (let ((v (truncate (limit (* 127 value) 0 127))))
				   (dolist (ci ci-list)
				     (push (cons time (midi-channel-pressure ci v)) acc))
				   (setf time (+ time delta-time))
				   (setf value (+ value delta-value))))
			       (reverse acc)))

	   (bend-handler (prt evn offset)
			 (setf evn (->alist (remove :bend evn)))
			 (let* ((orc (property prt :orchestra))
				(steps (parse-step-count evn))
				(time (+ offset (parse-time prt evn)))
				(time2 (+ offset (parse-end-time prt evn)))
				(delta-time (/ (- time2 time) steps))
				(start (parse-start-value evn))
				(end (parse-end-value evn))
				(delta-value (/ (- end start) steps))
				(acc '())
				(ci-list (let ((bcc '()))
					   (dolist (inst-name (->list (instruments prt)))
					     (let* ((inst (find-node orc inst-name))
						    (ci (1- (channel inst :resolve t))))
					       (push ci bcc)))
					   bcc))
				(value start))
			   (while (< time (+ time2 delta-time))
			     (let* ((v (bend->midi-data value))
				    (lsb (aref v 0))
				    (msb (aref v 1)))
			       (dolist (ci ci-list)
				 (push (cons time (midi-pitch-bend ci lsb msb)) acc))
			       (setf time (+ time delta-time))
			       (setf value (+ value delta-value))))
			   (reverse acc)))

	   (program-change-handler (prt evn offset)
	   			   (setf evn (->alist evn))
	   			   (let* ((orc (property prt :orchestra))
	   				  (prgnum (cdr (assoc :program evn)))
	   				  (bank (cdr (assoc :bank evn)))
	   				  (time (+ offset -0.01 (parse-time prt evn)))
	   				  (acc '()))
	   			     (dolist (inst-name (->list (instruments prt)))
	   			       (let* ((inst (find-node orc inst-name))
	   				      (cindex (1- (channel inst :resolve t)))
	   				      (hook (property inst :program-change-hook))
					      (pnum (or prgnum (property inst :program-number)))
					      (bnk (or bank (property inst :program-bank))))
					 (if pnum
					     (setf acc (append acc (funcall hook time cindex prgnum bnk))))))
	   			     acc)) )
      
      (defmethod render-once ((prt epart) &key (offset 0))
	(if (not (mute? prt))
	    (progn 
	      (reset-params)
	      (let ((acc '()))
		(dolist (event (events prt))
		  (cond ((find :chord event)
			 (setf acc (append acc (chord-handler prt event offset))))
			((find :key event)
			 (setf acc (append acc (note-handler prt event offset))))
			((find :cc event)
			 (setf acc (append acc (cc-handler prt event offset))))
			((find :pressure event)
			 (setf acc (append acc (pressure-handler prt event offset))))
			((find :bend event)
			 (setf acc (append acc (bend-handler prt event offset))))
			((find :program event)
			 (setf acc (append acc (program-change-handler prt event offset))))
			(t (let ((frmt "Malformed event epart ~A  event ~A"))
			     (error (format nil frmt (name prt) event))))))
		(sort acc #'sort-midi-events-test)))
	  nil)) )))

(defmethod clone ((p epart) &key
		  (newname "~A-clone")
		  (parent nil)
		  (hook #'identity))
  (let* ((new-name (format nil newname (name p)))
	 (new-parent (or parent (parent p)))
	 (other (make-instance 'epart
			       :parent new-parent
			       :name new-name
			       :instruments (instruments p)
			       :period (period p)
			       :mute (mute? p)))
	 (acc '()))
    (dolist (key (property-keys p :local))
      (property! other key (property p key)))
    (dolist (e (events p))
      (push (cons (car e)(clone (cdr e))) acc))
    (setf (events other)(reverse acc))
    (setf other (funcall hook other))
    (add-child! new-parent other)
    other))
		 
(defmethod transpose! ((p epart)(x t) &key (range '(0 127)))
  (if (transposable-p p)
      (dolist (e (events p))
	(let ((i (position :key e)))
	  (if i
	      (let* ((j (1+ i))
		     (k1 (nth j e))
		     (k2 (transpose k1 x :range range)))
		(setf (nth j e) k2))))))
  p)

(defmethod invert! ((p epart)(pivot t) &key (range '(0 127)))
  (if (transposable-p p)
      (dolist (e (events p))
	(let ((i (position :key e)))
	  (if i
	      (let* ((j (1+ i))
		     (k1 (nth j e))
		     (k2 (invert k1 pivot :range range)))
		(setf (nth j e) k2))))))
  p)
