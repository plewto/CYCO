;;;; cyco.util.debug
;;;;

(in-package :cyco)


(defun debug (frmt &rest args)
  "Print the word 'DEBUG' and then apply format to frmt and remaining arguments."
  (let ((str (apply #'format (append (list nil frmt) args))))
    (format t "DEBUG ~A~%" str)))


(let ((depth 0)
      (stack '())
      (enable t))

  (defun trace-enable (flag)
    (setf enable flag))

  (defun trace-reset ()
    (setf depth 0)
    (setf stack '()))
  
  (defun trace-enter (&optional txt)
    (if enable
	(let ((pad (tab depth)))
	  (push (->string txt) stack)
	  (format t "[~2D] --> Trace Enter ~A~S~%" depth pad txt)
	  (setf depth (1+ depth)))))

  (defun trace-exit (&optional return)
    (if enable
	(progn
	  (setf depth (max 0 (1- depth)))
	  (let ((pad (tab depth))
		(txt (pop stack)))
	    (format t "[~2D] <-- Trace Exit  ~A~S~%" depth pad txt))))
    return)

  (defun trace-marker (&optional txt)
    (if enable
	(let ((pad (tab (1- depth))))
	  (format t "[~2D] --- Trace Mark: ~A~A~%" (1- depth) pad txt))))

 
  )

