;;;; xyxo.comp.strummer
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


;;; chord event clauses
;;;    :time (x x x)
;;;    :key kn
;;;    :chord chrd
;;;    :strum delay   :strum (delay scale)
;;;    :direction (list...  :up :down :coin :permute)
;;;    :dur d         :dur (d variance)
;;;    :end-together  flag
;;;    :amp a         :amp (a scale variance)
;;;
;;; cc event
;;;    :time (x x x) :cc ctrl :value v
;;;
;;; bend event
;;;    :time (x x x) :bend b
;;;
;;; program event
;;;    :time (x x x) :program p
;;;    :time (x x x) :program p :bank b
;;;

(defmacro strummer (name instrument &key     ;; only single instrument
			 (period nil)
			 (qfn #'bar)
			 (amp-map #'identity)
			 (duration-map #'identity)
			 (project *project*)
			 (section nil)
			 (events '()))
  `(progn
     (validate-part-parents ,project ,section)
     (let* ((sec (or ,section (current-section ,project)))
	    (prt (make-instance 'strummer
				:name ',name
				:instruments (->instrument-list ,instrument ,project)
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
	 (acc '()))
    (flet ((parse-time (evn)
		       ;; :time (x x x)
		       (let ((spec (cdr (assoc :time evn))))
		       	 (set-if time (apply qfn spec))))
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
				(error (format nil "~A is not valid strummer chord (strummer ~A)"
					       spec (name prt)))))
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
				  (setf direction-pattern (cycle :of (->list spec))))
			      direction-pattern))
	   (parse-duration (evn)
			   ;; :dur d
			   ;; :dur (d variance)
			   (let ((spec (cdr (assoc :dur evn))))
			     (if spec
				 (progn
				   (setf spec (fill-list (->list spec)
							 (list duration
							       duration-variance)))
				   (setf duration (metric (car spec)))
				   (setf duration-variance (float (second spec)))))
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
				    (setf spec (fill-list (->list spec)
							  (list amplitude
								amplitude-scale
								amplitude-variance)))
				    (setf amplitude (amplitude (car spec)))
				    (setf amplitude-scale (float (second spec)))
				    (setf amplitude-variance (float (third spec)))))
			      (list amplitude amplitude-scale amplitude-variance)))
	   (handle-cc (evn)
		     ;; :cc ctrl :value v
		     (let ((cc (cdr (assoc :cc evn))))
		       (if cc
			   (let* ((norm-val (or (cdr (assoc :value evn)) 0.0))
				  (value (limit (truncate (* 127 norm-val)) 0 127))
				  (ctrl (controller-assignment cc :cmap cc-map)))
			     (push (cons time (midi-control-change channel-index
								   ctrl value))
				   acc)))))
	   (handle-bend (evn)
			;; :bend value
			(let ((norm (cdr (assoc :bend evn))))
			  (if norm
			      (let* ((bytes (bend->midi-data norm))
				     (lsb (aref bytes 0))
				     (msb (aref bytes 1)))
				(push (cons time (midi-pitch-bend channel-index
								  lsb msb))
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
			 (if (or (member :key evn :test #'(lambda (a b)(eq a (car b))))
				 (member :chord evn :test #'(lambda (a b)(eq a (car b)))))
			     (let* ((bcc '())
				    (chord-keylist (gtrchord keynumber chord))
				    (dir (next-1 direction-pattern))
				    (keylist (cond ((eq dir :up)
						    (reverse chord-keylist))
						    ((eq dir :down)
						     chord-keylist)
						    ((eq dir :coin)
						     (coin 0.5 chord-keylist (reverse chord-keylist)))
						    ((eq dir :randome)
						     (permute chord-keylist))
						    (t
						     (cyco-warning
						      (format nil "~A is not valid strummer chord direction." dir))
						     chord-keylist)))
				    (start-time time)
				    (current-time start-time)
				    (current-delay delay)
				    (current-amp (amplitude amplitude))
				    )
			       (dolist (kn (keynumber keylist))
				 (let* ((dur (approx (float (metric duration)) :scale duration-variance))
					(end-time (if end-together
						      (+ start-time dur)
						    (+ current-time dur)))
					(amp (instrument-amplitude
					      instrument current-amp))
					(vel (limit (truncate (* amp 127)) 1 127)))
				   (push (cons current-time (midi-note-on channel-index kn vel)) acc)
				   (push (cons end-time (midi-note-off channel-index kn 0)) acc)
				   (setf current-time (+ current-time current-delay))
				   (setf current-delay (* current-delay delay-scale))
				   (setf current-amp (approx (* current-amp amplitude-scale) :scale amplitude-variance)))))))
			 )
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


	   
	   
			      
