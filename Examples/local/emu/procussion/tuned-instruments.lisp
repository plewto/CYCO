;;;; emu procussion tuned-instruments
;;;;

(in-package :cyco)

(let ((tuned-keymap (default-keymap 'tuned-procussion-instrument)))
  (macrolet ((tuned-instrument (name parent)
			       `(param ,name
				       (create-instrument
					',name
					:parent ,parent
					:keynumber-map tuned-keymap
					:program-change-hook
					(constant-program-hook
					 ',name
					 (car
					  (cdr
					   (assoc
					    ',name
					    +PROCUSSION-PROGRAMS+))))))))
    (defun vibrations (&key (parent pro3))
      (tuned-instrument vibrations parent))

    (defun marimba (&key (parent pro3))
      (tuned-instrument marimba parent))

    (defun rosewood (&key (parent pro3))
      (tuned-instrument rosewood parent))

    (defun malletbells (&key (parent pro3))
      (tuned-instrument malletbells parent))

    (defun indo-steel (&key (parent pro3))
      (tuned-instrument indo-steel parent))

    (defun killer (&key (parent pro3))
      (tuned-instrument killer parent))

    (defun mystic-land (&key (parent pro3))
      (tuned-instrument mystic-land parent))

    (defun clavarimba (&key (parent pro3))
      (tuned-instrument clavarimba parent))

    (defun thundadome (&key (parent pro3))
      (tuned-instrument thundadome parent))

    (defun industry (&key (parent pro3))
      (tuned-instrument industry parent))

    (defun churchyard (&key (parent pro3))
      (tuned-instrument churchyard parent))

    (defun ritual-night (&key (parent pro3))
      (tuned-instrument ritual-night parent))

    (defun intervallix (&key (parent pro3))
      (tuned-instrument intervallix parent))

    (defun more-basses (&key (parent pro3))
      (tuned-instrument more-basses parent))))
