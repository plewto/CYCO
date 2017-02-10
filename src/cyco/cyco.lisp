;;;; cyco.cyco
;;;;

;; intentionally not in :cyco package
;;
(defun cyco ()
  (in-package :cyco)
  (format t "CYCO~%"))

(defpackage :cyco
  (:use common-lisp)
  (:import-from common-lisp *))

(in-package :cyco)

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

(let* ((cyco-boot "src/cyco/cyco")
       (current-file cyco-boot)
       (enable-rl t))

  (defun ld (filename &key (verbose nil)(print nil))
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

(format t "Loading cyco...~%")
(ld "src/cyco/constants")
(ld "src/cyco/globals")
(ld "src/cyco/generics")
(ld "src/cyco/util/utilities")
(ld "src/cyco/util/string-utilities")
(ld "src/cyco/util/list-utilities")
(ld "src/cyco/util/debug")
(ld "src/cyco/notification")
(ld "src/cyco/util/os")
(ld "src/cyco/midi/midi-util" )
(ld "src/cyco/midi/event")
(ld "src/cyco/midi/syscommon")
(ld "src/cyco/midi/meta")
(ld "src/cyco/midi/smf-header")
(ld "src/cyco/midi/smf-track")
(ld "src/cyco/midi/smf")
(ld "src/cyco/node")
(ld "src/cyco/patterns/pattern")
(ld "src/cyco/comp/amplitude" )
(ld "src/cyco/comp/chords")
(ld "src/cyco/comp/keynum")
(ld "src/cyco/comp/metric")
(ld "src/cyco/orch/channel")
(ld "src/cyco/orch/controller")
(ld "src/cyco/orch/keymaps")
(ld "src/cyco/orch/progmap")
(ld "src/cyco/orch/instrument")
(ld "src/cyco/orch/metronome")
(ld "src/cyco/comp/timesig" )
(ld "src/cyco/comp/project")
(ld "src/cyco/comp/section")
(ld "src/cyco/comp/group")
(ld "src/cyco/comp/part")
(ld "src/cyco/comp/fixed-part")
(ld "src/cyco/comp/epart")
(ld "src/cyco/comp/qball")
(ld "src/cyco/comp/controlball")
(format t "~%")
(ld "src/cyco/local")

(format t "~A~%" +BANNER+)
(format t "CYCO version ~A~%~%" +CYCO-VERSION+)


;;; Convenience functions
;;;

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

(setfn lp load-project)
(setfn lpf load-project-file)


;;; TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST
;;; TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST
;;; TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST

;; (project 'abc)
;; (section s)
;; (epart ep txa
;;        :events '((:time (1 1 1) :program 1 :bank B)
;; 		 (:time (1 2 1) :to (2 2 1) :pressure :start 0.0 :end 1.0 :steps 4)
;; 		 (:time (1 3 1) :to (2 3 1) :bend :start -1.0 :end 1.0 :steps 4)
;; 		 (:time (1 4 1) :to (2 4 1) :cc 1 :start 0.0 :end 1.0 :steps 4)
;; 		 (:time (2 1 1) :key 60 :dur q :amp ff)
;; 		 (:time (2 2 1) :key 60 :chord [maj])))
;;
;; (fixed-part fp :events '((time (1 1 1) :channel 1 :key 60)
;; 			 (time (1 2 1) :cc 1 :value 0.5)))
;;
;; (qball qb txa
;;        :cue '((1 1 1)(1 2 1)(1 3 1)(1 4 1))
;;        :key (cycle :of (list 60 61 (line :of '(62 63)))))
;;
;;
;; (?p 4)
