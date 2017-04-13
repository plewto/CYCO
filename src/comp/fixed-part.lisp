;;;; cyco.comp.fixed-part
;;;;
;;;; A FIXED-PART is a PART where MIDI events are specified directly.
;;;; Unlike all other sub-classes of PART, FIXED-PART does not take
;;;; a list of INSTRUMENTs.
;;;;

(in-package :cyco)

(defgeneric --parse-fixed-part-events (prt qfn events))

(defclass fixed-part (part) nil)
  
(defmethod --parse-fixed-part-events ((prt fixed-part)
				      (qfn function)
				      (events list))
  (let ((chan-map (property prt :channel-assignments))
	(ctrl-map (property prt :controller-assignments))
	(default-time 0.0)
	(default-channel-index 0)
	(default-duration (metric 'q))
	(default-amplitude (amplitude 'mf))
	;(default-ctrl 1)
	(default-cc-value 0.0)
	(msg1 "Expected even number of elements in fixed-pattern event list")
	(acc '()))
    (flet ((->alist (lst)
		    (let ((bcc '())
			  (i 0))
		      (if (not (evenp (length lst)))
			  (progn
			    (cyco-warning msg1)
			    (error (format nil "fixed-part ~A" (name prt)))))
		      (while (< i (length lst))
			(push (cons (nth i lst)(nth (1+ i) lst)) bcc)
			(setf i (+ i 2)))
		      bcc))
	   
	   (parse-time (evn)
		       (let* ((time-spec (cdr (assoc :time evn)))
			      (time (or (and time-spec 
					     (apply qfn time-spec))
					default-time)))
			 (setf default-time time)
			 time))
	  
	   (parse-channel-index (evn)
				(let* ((c-spec (cdr (assoc :channel evn)))
				       (ci (or (and c-spec
						    (1- (channel-assignment
							 c-spec
							 :cmap chan-map
							 :resolve t)))
					       default-channel-index)))
				  (setf default-channel-index ci)
				  ci))

	   (parse-duration (evn)
			   (let* ((d-spec (cdr (assoc :dur evn)))
				  (d (or (and d-spec (parse-metric-expression d-spec))
					 default-duration)))
			     (setf default-duration d)
			     d))

	   (parse-velocity (evn)
			   (let* ((a-spec (cdr (assoc :amp evn)))
				  (a (or (and a-spec (amplitude a-spec))
					 default-amplitude))
				  (velocity (truncate (* 127 a))))
			     (setf default-amplitude a)
			     velocity)) )
			 
      (flet ((note-handler (evn)
			   (setf evn (->alist evn))
			   (let* ((time1 (parse-time evn))
				  (cindex (parse-channel-index evn))
				  (kn (keynumber (cdr (assoc :key evn))))
				  (dur (parse-duration evn))
				  (vel (parse-velocity evn))
				  (time2 (+ time1 dur)))
			     (push (cons time1 (midi-note-on cindex kn vel)) 
				   acc)
			     (push (cons time2 (midi-note-off cindex kn 0)) 
				   acc)))
	     
	     (cc-handler (evn)
	     		 (setf evn (->alist evn))
	     		 (let* ((time (parse-time evn))
				(cindex (parse-channel-index evn))
				(ctrl (controller-assignment
				       (cdr (assoc :cc evn))
				       :cmap ctrl-map))
				(value (or (cdr (assoc :value evn))
					   default-cc-value)))
	     		   (setf default-cc-value value)
	     		   (setf value (truncate (* 127 value)))
	     		   (push (cons time 
				       (midi-control-change cindex ctrl value)) 
				 acc)))
	     
	     (pressure-handler (evn)
			       (setf evn (->alist evn))
			       (let* ((time (parse-time evn))
				      (cindex (parse-channel-index evn))
				      (value (cdr (assoc :pressure evn)))
				      (prs (truncate (* 128 value))))
				 (push (cons time (midi-channel-pressure 
						   cindex prs)) acc)))

	      (bend-handler (evn)
			       (setf evn (->alist evn))
			       (let* ((time (parse-time evn))
				      (cindex (parse-channel-index evn))
				      (norm (float (cdr (assoc :bend evn))))
				      (bytes (bend->midi-data norm))
				      (lsb (aref bytes 0))
				      (msb (aref bytes 1)))
				 (push (cons time (midi-pitch-bend 
						   cindex lsb msb)) acc)))
	     
	      (program-handler (evn)
			       (setf evn (->alist evn))
			       (let* ((time (parse-time evn))
				      (cindex (parse-channel-index evn))
				      (val (cdr (assoc :program evn)))
				      (prognum (truncate val)))
				 (push (cons time (midi-program-change
						   cindex prognum)) acc))) )
	(dolist (evn events)
	  (cond ((eq :key (find :key evn))(note-handler evn))
		((eq :cc (find :cc evn))(cc-handler evn))
		((eq :pressure (find :pressure evn))(pressure-handler evn))
		((eq :bend (find :bend evn))(bend-handler evn))
		((eq :program (find :program evn))(program-handler evn))
		(t (let ((frmt "Fixed-pattern ~A event missing command:  ~A"))
		     (error (format nil frmt (name prt) evn))))))
	acc))))
  


(defmacro fixed-part (name &key
			   (period nil)
			   (qfn #'bar)
			   (project *project*)
			   (section nil)
			   (events '()))
  "Creates new instance of FIXED-PART
   name     - Symbol, the part's name.  The new part is bound to name.
   :period  - Duration of part in seconds. Defaults to duration of
              the parent SECTION.  If period is less then the section
              duration, the part events are repeated when the section
              is rendered.
   :qfn     - Time cuing function, default #'BAR
   :project - Default *PROJECT*
   :section - Default current SECTION of project.
   :events  - List of events with format:
              
              '((:TIME (br bt sb) :CHANNEL c <command> ...)
                (:TIME (br bt sb) :CHANNEL c <command> ...)
                ....)

             :TIME argument should be in a format expected by the 
             cuing function qfn (#'BAR format shown).

             :CHANNEL - MIDI channel 
             <command> is one of :KEY :CC :PRESSURE :BEND or :PROGRAM

             :KEY       (:TIME (br bt sb) :CHANNEL c :KEY k :DUR d :AMP a
                        Generate note on/off events.

                        k - keynumber  - MIDI key number or symbolic.
                        d - duration   - Time in seconds or symbolic metric
                                         value W H Q etc...
                        a - amplitude  - Normalized amplitude between 
                                         0..1 or symbolic PP P MF F etc...
                                         Amplitude is converted to MIDI 
                                         velocity.

                        The values for :TIME :CHANNEL :DUR and :AMP are 
                        optional. If not specified they default to the 
                        corresponding value of the previous event.

             :CC        (:TIME (br bt sb) :CHANNEL c  :CC ctrl :VALUE v) 
                        Generates MIDI control-change events.

                        ctrl  - MIDI controller
                        value - Normalized controller value (0..1)
     
                        :TIME :CHANNEL and :VALUE are optional.  If not
                        specified they default to corresponding value of
                        previous event.

             :PRESSURE  (:TIME (br bt sb) :CHANNEL c :PRESSURE P)
                        Generates MIDI channel-pressure event

                        p - Normalized pressure value (0..1)

                        :TIME and :CHANNEL are optional.

             :BEND      (:TIME (br bt sb) :BEND b)
                        Generates MIDI pitch bend event.

                        b - Normalized bend (-1..+1)

                        :TIME and :CHANNEL are optional.

             :PROGRAM   (:TIME (br bt sb) :CHANNEL c :PROGRAM n)
                        Generate MIDI program-change event.

                        n - Program number (0,1,2,...,127)

                        :TIME and :CHANNEL are optional."
  `(progn
     (validate-part-parents ,project ,section)
     (let* ((sec (or ,section (current-section ,project)))
	    (prt (make-instance 'fixed-part :name ',name
				:period (or ,period (duration sec)))))
       (property! prt :transposable t)      
       (add-child! sec prt)
       (part-banner sec prt)
       (param ,name prt)
       (setf (events prt)
	     (--parse-fixed-part-events prt ,qfn ,events))
       prt)))
     

(defmethod render-once ((prt fixed-part) &key (offset 0))
  (if (not (mute? prt))
      (let ((proj (parent (parent prt)))
	    (events (sort (clone (events prt))
			  #'(lambda (a b)
			      (< (car a)(car b)))))
	    (acc '()))
	(setf (current-section proj)(parent prt))
	(dolist (ev events)
	  (push (cons (+ (car ev) offset)(cdr ev)) acc))
	acc)
    nil))

(defmethod dump ((prt fixed-part) &key (depth 0)(max-depth 10))
  (if (< depth max-depth)
      (if (mute? prt)
	  (format t "~AMUTED ~A ~A~%" (tab depth)(type-of prt)(name prt))
	(let ((pad (tab (1+ depth))))
	  (format t "~A~A ~A~%" (tab depth)(type-of prt)(name prt))
	  (if (< (1+ depth) max-depth)
	      (progn
		(setf (events prt)
		      (sort (events prt) #'(lambda (a b)(< (car a)(car b)))))
	      (dolist (evn (events prt))
		(let ((time (car evn))
		      (msg (cdr evn)))
		  (format t "~A~7,4f  ~A~%" pad time (->string msg)))))))))
  nil)

(defmethod clone ((p fixed-part) &key
		  (newname "~A-clone")
		  (parent nil)
		  (hook #'identity))
  (let* ((new-name (format nil newname (name p)))
	 (new-parent (or parent (parent p)))
	 (other (make-instance 'fixed-part
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

(defmethod transpose! ((p fixed-part)(x t) &key (range '(0 127)))
  (if (transposable-p p)
      (dolist (e (events p))
	(let ((evn (cdr e)))
	  (if (key-event-p evn)
	      (setf (cdr e)(transpose evn x :range range))))))
  p)
    
(defmethod invert! ((p fixed-part)(pivot t) &key (range '(0 127)))
  (if (transposable-p p)
      (dolist (e (events p))
	(let ((evn (cdr e)))
	  (if (key-event-p evn)
	      (setf (cdr e)(invert evn pivot :range range))))))
  p)
