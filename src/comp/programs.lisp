;;;; cyco.comp.part
;;;;
;;;; PROGRAMS is a specialized PART for MIDI program-change events.

(in-package :cyco)

(defclass programs (part) nil)

(defmacro programs (name time instruments &key
			 (qfn #'bar)
			 (project *project*)
			 (section nil))
  "Creates instance of PROGRAMS.   
   PROGRAMS is a subclass of PART used to generate MIDI program-change events.
   name        - Symbol, the parts name.   The new part is bound to name. 
   time        - Time specification must be in format expected by qfn.
                 For the default where qfn is #'bar this is 
                 (beat bar subbeat tick).
   instruments - List of instruments of instrument names.
   :qfn        - Cuing function, default #'BAR
   :project    - Default *PROJECT*
   :section    - Default current section of project.

   Rendering PROGRAMS produces a series of MIDI events used to set up 
   proper banks/programs for each instrument."
  `(progn
     (validate-part-parents ,project ,section)
     (let* ((section (or ,section (current-section ,project)))
	    (prt (make-instance 'programs
				:name ',name
				:instruments (->instrument-list ,instruments ,project)
				:transposable nil
				:period (duration section)
				:parent ,section
				:transient t)))
       (add-child! section prt)
       (property! prt :time (apply ,qfn ,time))
       (param ,name prt)
       prt)))

(defmethod render-once ((p programs) &key (offset 0))
  (if (not (mute? p))
      (let ((acc '())
	    (time (+ offset (property p :time)))
	    (proj (parent (parent p))))
	(setf (current-section proj) (parent p))
	(dolist (inst (instruments p))
	  (let ((cindex (1- (channel inst :resolve t)))
		(pchook (property inst :program-change-hook))
		(pnum (property inst :program-number))
		(bnk (property inst :program-bank)))
	    (setf acc (append acc (funcall pchook time cindex pnum bnk)))))
	acc)
    nil))
		    
(defmethod transposable-p ((p programs)) nil)

