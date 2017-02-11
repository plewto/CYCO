;;;; cyco.notification
;;;;

(in-package :cyco)

(defun cyco-warning (&rest args)
  "Display warning message."
  (dolist (a args)
    (format t "WARNING: ~A~%" a)))

(constant +BAR1+ (scopies 75 #\*))
(constant +BAR2+ (scopies 4 #\*))

(constant +BAR3+ (scopies 55 #\-))
(constant +BAR4+ (scopies 4 #\-))

(defun banner (headlines &rest args)
  "Display top-level banner."
  (format t "~%~A~%" +BAR1+)
  (dolist (line (->list headlines))
    (format t "~A~%" (center-string (->string line) :width (length +BAR1+) :prefix +BAR2+)))
  (format t "~A~%" +BAR2+)
  (dolist (line args)
    (format t "~A ~A~%" +BAR2+ line))
  (format t "~%"))

(defun section-banner (section)
  "Display section-level banner."
  (format t "~%~A Section ~A~%" +BAR3+ (name section))
  (format t "~%"))
  
(defun part-banner (section part)
  "Display part-level banner."
  (format t "~A Section ~A Part ~A~%"
	  +BAR4+ (name section)(name part)))
