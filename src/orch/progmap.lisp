;;;; cyco.orch.progmap
;;;;
;;;; A program-change-hook is a function which generates events in response
;;;; to a program change.  The results are not always a simple program-change
;;;; event.   The results may contain bank-selection events or even note
;;;; events for use with "keyswitched" instruments.
;;;;
;;;; Another common usage is for the conversion of a synthesizers notion
;;;; of a program number and the actual value.   For instance the Korg R3
;;;; organizes it's programs in banks A, B, C, ...  with 8 program slots
;;;; numbered 1 through 8.
;;;;
;;;; program-change-hooks have the form (lambda (time cindex prognum bank))
;;;; where time    - The event time in seconds.
;;;;       cindex  - MIDI channel index (0..15)
;;;;       prognum - program number, however the specific instrument defines
;;;;                 them
;;;;       bank     - bank number, however they are defined.
;;;;
;;;; For the special case where prognum is :?  the function should display
;;;; useful help text and return nil.  When the function generates events they
;;;; should be a list of form ((time1 . event1)(time2 . event2)...)
;;;;

(in-package :cyco)

;;; Basic program-change, ignores bank.
;;;
(defun default-program-change-hook (time cindex program bank)
  "The default program change hook.
   time - real
   cindex - fixnum (0..15)
   program - fixnum (0..127) or :?
   bank - ignored.
   Returns single program change event.
   ((time . event))"
  (cond ((eq program :?)
	 (progn
	   (format t "default-program-change-map~%")
	   nil))
	(t (list (cons time (midi-program-change cindex program))))))

(defun program-hook-with-bank-select (iname &key (bank-ctrl 0)(delay 0.005))
  "Creates program-change-hook using MIDI CC 0 as bank select."
  #'(lambda (time cindex program bank)
      (if (eq program :?)
	  (progn
	    (format t "~A program-hook-with-bank~%" iname)
	    nil)
	(if bank
	    (list (cons time (midi-control-change cindex bank-ctrl bank))
		  (cons (+ time delay)(midi-program-change cindex program)))
	  (list (cons time (midi-program-change cindex program)))))))

(defun constant-program-hook (iname program-number)
  "Creates program-change-hook with constant result.
   program and bank arguments are ignored."
  #'(lambda (time cindex _1 _2)
      (if (eq program-number :?)
	  (progn
	    (format t "~A constant-program-hook ~A~%" iname program-number)
	    nil)
	(list (cons time (midi-program-change cindex program-number))))))

(defun keyswitch (iname alist &optional (delay 0.01))
  "Returns program-change-hook for use with 'keyswitch' instruments.
   Program changes are translated into specific note events.
   iname - instrument name, used only for help text
   alist - assoc list maps symbols to MIDI key numbers.
   delay - optional delay between note on and off events. Default 0.01.
   
   Returns function (lambda (time cindex program bank))
      time - event time
      cindex - MIDI channel index
      program - symbol, as defined in alist.
      bank - ignored.

      The hook functions returns list
      ((time . note-on)((+ time delay) note-off))"

  (flet ((gen-events (time cindex keynum)
		     (let ((kn (keynumber keynum)))
		       (list (cons time (midi-note-on cindex kn 1))
			     (cons (+ time delay)(midi-note-off cindex kn 0)))))
	 (doc ()
	      (format t "~A keyswitch~%" iname)
	      (let ((n 0))
		(dolist (a alist)
		  (format t "    ~2D [~12A] -> ~A~%" n (car a)(cdr a))
		  (setf n (1+ n))))
	      nil))
    #'(lambda (time cindex prognum &optional _)
	(let ((asc (assoc prognum alist)))
	  (cond ((eq prognum :?)(doc))
		(asc
		 (gen-events time cindex (car (cdr asc))))
		((integerp prognum)
		 (gen-events time cindex (car (cdr (cnth prognum alist)))))
		((keynumber-p prognum)
		 (gen-events time cindex prognum))
		(t
		 (let ((frmt "~A is invalid keyswitch for ~A"))
		   (warning (format nil frmt prognum iname))
		   nil)))))))
