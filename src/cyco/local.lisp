;;;; cyco.local
;;;;
;;;; Defines functions for loading local configuration files.
;;;;

(in-package :cyco)

(setf *cyco-config-directory*  (join-path (home-dir) ".config" "cyco"))
(setf *cyco-config-file* "local-settings.lisp")

(defun load-local (fname &key (verbose nil)(print nil))
  "Load local cyco configuration file.
   fname    - Local filename relative to *CYCO-CONFIG-DIRECTORY*.
   :verbose - Flag, if true display message that file is being loaded.
   :print   - Flag, pass as print argument to CL LOAD function."
  (let ((fqn (join-path *cyco-config-directory* fname))
	(temp *load-print*))
    (if verbose (format t "--- config ~A~%" fqn))
    (setf *load-print* print)
    (load fqn)
    (setf *load-print* temp)))

(defun reload-local ()
  "Load top-level configuration file <path>/<file>.lisp
   Where <path> is contained in *CYCO-CONFIG-DIRECTORY*
   and <file> is the value of *CYCO-CONFIG-FILE*"
  (if (file-exists (join-path *cyco-config-directory* *cyco-config-file*))
      (progn
	(format t "Local setting '~A' ~%" *cyco-config-file*)
	(format t "Loading local config...~%" *cyco-config-file*)
	(load-local *cyco-config-file*))
    (let ((msg "Config file '~A' does not exists!"))
      (warning (format nil msg *cyco-config-file*)) )))

(reload-local)
