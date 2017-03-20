;;;; cyco.cyco
;;;;

;; intentionally not in :cyco package
;;
(defun cyco ()
  (in-package :cyco)
  (format t "CYCO under ~A ~A~%"
	  (lisp-implementation-type)
	  (lisp-implementation-version)))

(defpackage :cyco
  (:use common-lisp)
  (:import-from common-lisp *))

(in-package :cyco)

(defun dismiss (&rest args) args)

(defmacro constant (name value &optional docstring)
  "An alias for defconstant."
  `(if (not (boundp ',name))
       (progn 
	 (defconstant ,name ,value ,docstring))))

(defmacro param (name &optional (value nil)(docstring ""))
  "An alias for defparameter."
  `(defparameter ,name ,value ,docstring))

(defmacro global (name &optional (value nil)(docstring ""))
  "An alias for defparameter."
  `(defparameter ,name ,value ,docstring))

(defmacro setfn (a b)
  "Sets function-value of symbol a to the function-value of symbol b."
  `(setf (symbol-function ',a)
	 (symbol-function ',b)))

(defmacro set-if (location value)
  "Change value of location if value is non nil.
   Returns value of location."
  `(progn
     (if ,value (setf ,location ,value))
     ,location))

(defun tab (&optional (n 1))
  (let ((frmt (format nil "~~~DA" (* 4 n))))
    (format nil frmt "")))

(let* ((cyco-boot "src/cyco")
       (current-file cyco-boot)
       (enable-rl t))

  (defun ld (filename &key (verbose t)(print nil))
    "Loads CYCO source file.  If verbose is true display message.
     Pass the value of print to the CL load function."
    (let ((temp *load-print*))
      (setf current-file filename)
      (if verbose
	  (format t "------------ Loading ~A~%" filename))
      (setf *load-print* print)
      (load filename)
      (setf *load-print* temp)
      current-file))
  
  (defun rl (&optional all)
    "Reload the previously loaded source file.  If all is true
     reload all CYCO source files."
    (if enable-rl
	(progn 
	  (if all (setf current-file cyco-boot))
	  (ld current-file))
      (format t "rl function is disabled~%")))

  (defun rla ()
    "Reloads all CYCO source files."
    (rl t)))

(param src-manifest
       '("src/constants"
	 "src/globals"
	 "src/generics"
	 "src/util/utilities"
	 "src/util/string-utilities"
	 "src/util/list-utilities"
	 "src/util/debug"
	 "src/notification"
	 "src/util/os"
	 "src/midi/midi-util" 
	 "src/midi/event"
	 "src/midi/syscommon"
	 "src/midi/meta"
	 "src/midi/smf-header"
	 "src/midi/smf-track"
	 "src/midi/smf"
	 "src/node"
	 "src/patterns/pattern"
	 "src/comp/amplitude" 
	 "src/comp/chords"
	 "src/comp/keynum"
	 "src/comp/metric"
	 "src/orch/channel"
	 "src/orch/controller"
	 "src/orch/keymaps"
	 "src/orch/progmap"
	 "src/orch/instrument"
	 "src/orch/metronome"
	 "src/comp/timesig" 
	 "src/comp/project"
	 "src/comp/section"
	 "src/comp/group"
	 "src/comp/part"
	 "src/comp/fixed-part"
	 "src/comp/epart"
	 "src/comp/qball"
	 "src/comp/metronome"
	 "src/comp/marker"
	 "src/comp/qlist"
	 "src/comp/controlball"
	 "src/comp/gtrchords"
	 "src/comp/strummer"))
	 

(format t "Loading cyco...~%")
;; (ld "src/constants")
;; (ld "src/globals")
;; (ld "src/generics")
;; (ld "src/util/utilities")
;; (ld "src/util/string-utilities")
;; (ld "src/util/list-utilities")
;; (ld "src/util/debug")
;; (ld "src/notification")
;; (ld "src/util/os")
;; (ld "src/midi/midi-util" )
;; (ld "src/midi/event")
;; (ld "src/midi/syscommon")
;; (ld "src/midi/meta")
;; (ld "src/midi/smf-header")
;; (ld "src/midi/smf-track")
;; (ld "src/midi/smf")
;; (ld "src/node")
;; (ld "src/patterns/pattern")
;; (ld "src/comp/amplitude" )
;; (ld "src/comp/chords")
;; (ld "src/comp/keynum")
;; (ld "src/comp/metric")
;; (ld "src/orch/channel")
;; (ld "src/orch/controller")
;; (ld "src/orch/keymaps")
;; (ld "src/orch/progmap")
;; (ld "src/orch/instrument")
;; (ld "src/orch/metronome")
;; (ld "src/comp/timesig" )
;; (ld "src/comp/project")
;; (ld "src/comp/section")
;; (ld "src/comp/group")
;; (ld "src/comp/part")
;; (ld "src/comp/fixed-part")
;; (ld "src/comp/epart")
;; (ld "src/comp/qball")
;; (ld "src/comp/metronome")
;; (ld "src/comp/marker")
;; (ld "src/comp/qlist")
;; (ld "src/comp/controlball")
;; (ld "src/comp/gtrchords")
;; (ld "src/comp/strummer")

(dolist (s src-manifest)
  (ld s))

(format t "~%")
(ld "src/local")

(format t "~A~%" +BANNER+)
(format t "CYCO version ~A~%~%" +CYCO-VERSION+)

(defun compile-cyco nil
  (dolist (s src-manifest)
    (format t "Compiling ~A~%" s)
    (compile-file s)))
    

;;; Convenience functions


(setfn ?a apropos)

(defun ?o (&optional (node *root-instrument*))
  "Display orchestra tree for current project."
  (dump node))

(defun ?p(&optional (depth 1))
  "Display project tree (for *PROJECT*)
   depth - controls verbosity
           1 -> project, sections.
           2 -> project, sections, parts.
           3 -> project, sections, parts, events."
  (dump *project* :max-depth (+ 2 depth)))

(defun ?km (inst)
  "Display information about an instruments keynumber map."
  (let ((kmap (property inst :keynumber-map)))
    (if kmap
	(funcall kmap :?)
      (format t "No keymap defined for ~A.~%" (name inst)))))

(defun ?i (inst-spec)
  "Display information about an instrument."
  (let* ((orc *root-instrument*)
	 (inst (or (and (root-p inst-spec) inst-spec)
		   (find-node orc inst-spec))))
    (format t "Instrument ~A   channel ~A(~A)~%"
	    (name inst)(channel inst)(channel inst :resolve t))
    (format t "    parent        ~A~%" (name (parent inst)))
    (format t "    remarks       ~A~%" (remarks inst))
    (format t "    transient     ~A~%" (is-transient inst))
    (format t "    keynumber-map ~A~%" (property inst :keynumber-map))
    (format t "    duration-map  ~A~%" (property inst :duration-map))
    (format t "    amplitude-map ~A~%" (property inst :amplitude-map))
    (format t "    program-hook  ~A~%" (property inst :program-change-hook))
    (format t "    program       ~A~%" (property inst :program-number))
    (format t "    bank          ~A~%" (property inst :program-bank))
    (funcall (property inst :keynumber-map) :?)
    (funcall (property inst :duration-map) :?)
    (funcall (property inst :amplitude-map) :?)
    (funcall (property inst :program-change-hook) 0 0 :? nil)))

(defun ?chords (&optional (silent nil))
  "Display list of defined chords in current project.
   If silent is true, return list without printing."
  (let* ((cd (property *project* :chord-dictionary))
	 (keys (property-keys cd))
	 (acc '()))
    (dolist (k keys)
      (push (cons k (property cd k)) acc))
    (setf acc (sort acc #'(lambda (a b)(string> (->string (car a))
						(->string (car b))))))
    (if (not silent)
	(progn
	  (dolist (c acc)
	    (format t "~12A -> ~A~%" (car c)(cdr c)))
	  nil)
      acc)))
