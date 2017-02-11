;;;; cyco.comp.project
;;;;
;;;; Defines the PROJECT class.
;;;; A PROJECT consist of a series of sequential SECTIONS.  Projects also
;;;; maintain an orchestra, default TIMESIG, MIDI channel and controller
;;;; assignments.
;;;;
;;;; At any one time there is a "current" section.  Many cyco functions
;;;; default to use the current section of the global *PROJECT*.


(in-package :cyco)

(setf *cyco-projects-directory* (join-path (home-dir) "Documents" "cyco-projects"))
(setf *default-project-boot-file* "main")
(setf *default-project-midi-directory* "MIDI")
(setf *project* nil)


(defclass project (node)
  ((timesig
    :type timesig
    :accessor project-timesig
    :initform nil
    :initarg :timesig)
   (current-section
    :type nil
    :accessor current-section
    :initform nil)
   (seqeunce-order
    :type list
    :accessor sequence-order
    :initform '())))
    

(defmethod project-p ((p project)) p)

(defmethod add-child! ((p project)(s node) &key
		       (test #'(lambda (a b) (dismiss a b) t)))
  (dismiss test)
  (if (section-p s)
      (progn
	(push-end s (children p))
	(push-end s (sequence-order p))
	(setf (parent s) p))
    (let ((frmt "Can not add non-child object ~A to project"))
      (error (format nil frmt s)))))

(defgeneric --project-filename (obj))
(defmethod --project-filename ((s string)) s)
(defmethod --project-filename ((s symbol))(string-downcase (symbol-name s)))

(defun project (name &key
		     (title nil)
		     (catalog-number "")
		     (directory nil)
		     (midi-directory nil)
		     (boot-file *default-project-boot-file*)
		     (channel-assignments *global-channel-assignments*)
		     (controller-assignments *global-controller-assignments*)
		     (orchestra nil)
		     (timesig (timesig :parent *root-timesig*
				       :name :project-timesig)))
  "Creates new PROJECT object and binds it to the global *PROJECT* variable.
   name - symbol.  In addition to naming the project the name is used to 
          locate project files.   The basic path for the primary project 
          file is    
                     <cp>/<name>/<boot>.lisp

           where <cp>   value of *CYCO-PROJECTS-DIRECTORY*
                 <name> is the lowercase symbol-name of name.
                 <boot> is the value of *DEFAULT-PROJECT-BOOT-FILE*
                        which defaults to 'main.lisp'.
        
           So for project 'FOO the main file is <cp>/foo/main.lisp
           All other project files are relative to <cb>/<name>.
    :title - String - The actual composition title (defaults to name)
    :catalog-number - Solely for end users use.
    :directory - defaults to *DEFAULT-PROJECT-DIRECTORY*
    :midi-directory - name of MIDI directory under the project directory.
            defaults to *DEFAULT-PROJECT-MIDI-DIRECTORY* ('MIDI')
    :boot-file - defaults to *DEFAULT-PROJECT-BOOT-FILE* ('main')
    :channel-assignments - defaults to *GLOBAL-CHANNEL-ASSIGNMENTS*
    :controller-assignments - defaults to *GLOBAL-CONTROLLER-ASSIGNMENTS*
    :orchestra - defaults to *ROOT-INSTRUMENT*
    :timesig - defaults to inheriting from ROOT-TIMESIG*"
  (let* ((proj (make-instance 'project
			     :name name
			     :timesig timesig))
	 ;(orch-root nil)
	 )
    (property! proj :title (->string (or title name)))
    (property! proj :catalog-number (->string catalog-number))
    (property! proj :directory (or directory
				   (join-path *cyco-projects-directory*
					      (--project-filename name))))
    (property! proj :midi-directory (or midi-directory
					(join-path (property proj :directory)
						   *default-project-midi-directory*)))
    (property! proj :boot-file (or boot-file *default-project-boot-file*))
    (property! proj :channel-assignments channel-assignments)
    (property! proj :controller-assignments controller-assignments)
    (property! proj :current-project-filename (property proj :boot-file))
    ;; (setf orch-root (create-instrument
    ;; 		     'root
    ;; 		     :channel-assignments channel-assignments
    ;; 		     :controller-assignments controller-assignments
    ;; 		     :channel 1))
    (property! proj :chord-dictionary (chord-dictionary))
    (property! proj :orchestra (or orchestra *root-instrument*))
    (setf *project* proj)
    (banner (format nil "Project ~A" name))
    proj))

;; (defmethod add-child! ((n project)(child node) &key
;; 		       (test #'(lambda (nd ch) t)))
;;   (if (section-p child)
;;       (call-next-method)
;;     (let ((frmt "Can not add non-section as project node.  ~A"))
;;       (error (format nil frmt child)))))

(defun assert-current-project ()
  "Produce error if *PROJECT* is not bound to a PROJECT object."
  (or (project-p *project*)
      (error "There is no current project!")))

(defun sections (&optional (project *project*))
  "Returns list of section in this project."
  (if (project-p project)
      (children project)
    nil))

(defun load-project (project-name &key
				  (dir *cyco-projects-directory*)
				  (boot-file *default-project-boot-file*))
  "Load the main project boot file.
   project-name - If not specified reload the current project.
   As convenience LP is an alias for LOAD-PROJECT"
  (if (not project-name)
      (progn
	(assert-current-project)
	(setf project-name (name *project*))))
  (let ((fqn (join-path dir (--project-filename project-name) boot-file)))
    (format t "Loading project \"~A\"~%" fqn)
    (load fqn)))

(defun load-project-file (&optional filename (project *project*))
  "Load a file from the project directory.
   If filename is not specified, reload the previously load file.
   As a convenience LPF is an alias for LOAD-PROJECT-FILE"
  (if (project-p project)
      (let* ((fn (--project-filename (or filename
					 (property project :current-project-filename))))
	     (fqn (join-path (property project :directory) fn)))
	(property! project :current-project-filename fn)
	(format t "Loading \"~A\"~%" fqn)
	(load fqn))
    (error (format t "load-project-file Invalid project"))))

(defmethod reset ((p project))
  (dolist (s (sections p))
    (reset s)))

(defmethod duration ((p project))
  "Returns duration of PROJECT in seconds.
   The duration is the sum of all (non-skipped) SECTION durations."
  (let ((acc 0))
    (dolist (s (sequence-order p))
      (setf acc (+ acc (duration s))))
    acc))

(defun orchestra (&key (project *project*)(print t))
  "Return project's orchestra."
  (if (project-p project)
      (let ((root (property project :orchestra)))
	(if print (dump root))
	root)
    nil))

;; Convert list of instrument names to actual instrument nodes.
;; instruments arg may be a single object or list.
;; Generate error if an instrument is not found.
;; Returns list.
;;
(defun extract-instruments (instruments &key (project *project*))
  "Return list of instruments from the current project."
  (let ((acc '())
	(orc (property project :orchestra)))
    (dolist (iname (->list instruments))
      (let ((inst (find-node orc iname)))
	(if (not inst)
	    (let ((frmt "Orchestra does not contain instrument ~A"))
	      (error (format nil frmt iname))))
	(push inst acc)))
    (reverse acc)))

(defun seq-order (sqlist &key (project *project*))
  (let ((acc '()))
    (dolist (sname sqlist)
      (let ((sec (get-child project sname)))
	(if sec
	    (push sec acc)
	  (let ((frmt "Project ~A does not have Section ~A"))
	    (cyco-warning (format nil frmt (name project) sname))))))
    (setf (sequence-order project) (reverse acc))
    project))


(defun render-project (&key (project *project*) 
			    (offset 0)
			    (write-midi-file t)
			    (midi-filename nil)
			    (pad-end 2.0))
  "Render projects MIDI events and optionally save to MIDI file.
   project - default *PROJECT*
   offset - time shift added to events, default 0.
   write-midi-file - boolean, if true write project events to MIDI file 
   midi-filename - defaults to project name with .mid extension.
   pad-end - amount of time to extend MIDI file after final event."
  (let ((events (if (project-p project)
		    (let ((acc '())
			  (time offset))
		      (dolist (s (sequence-order project))
			(setf acc (append acc (render-once s :offset time)))
			(setf time (+ time (duration s))))
		      (sort acc #'(lambda (a b)(< (car a)(car b)))))
		  (error "No current project"))))
    (if write-midi-file
    	(let* ((smf (smf :initial-tempo (tempo (project-timesig project))))
    	       (trk (car (smf-tracks smf)))
	       (fname (or midi-filename
			  (string-downcase (str+ (name project) ".mid")))))
    	  (dolist (evn events)
    	    (push-event (car evn)(cdr evn) trk))
    	  (save-smf smf
		    (join-path (property project :midi-directory) fname)
    		    :pad-end pad-end)
    	  smf)
      events)))

(defmethod ->midi ((proj project) &key filename (offset 0)(repeat :ignore)(pad-end 2))
  "Render and save project to MIDI file."
  (dismiss repeat)
  (render-project :project proj
		  :offset offset
		  :write-midi-file t
		  :midi-filename filename
		  :pad-end pad-end))

(defmethod ->midi ((obj null) &key filename (offset 0)(repeat :ignore)(pad-end 2))
  "Render and save *PROJECT* to MIDI file."
  (dismiss repeat)
  (assert-current-project)
  (->midi *project*
	  :filename filename
	  :offset offset
	  :pad-end pad-end))

(defmethod dump ((p project) &key (depth 1)(max-depth 10))
  (if (< depth max-depth)
      (progn
	(format t "~AProject ~A~%" (tab depth)(name p))
	(dolist (s (sequence-order p))
	  (dump s :depth (1+ depth) :max-depth max-depth))))
  nil)

