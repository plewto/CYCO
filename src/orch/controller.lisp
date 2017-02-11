;;;; cyco.orch.controller
;;;;
;;;; The CONTROLLER-ASSIGNMENT-MAP is used to assign names to MIDI
;;;; continuous controllers. Each PROJECT uses *GLOBAL-CONTROLLER-ASSIGNMENTS*
;;;; as a default.
;;;;


(in-package :cyco)

(defclass controller-assignment-map (node) nil)

(defun controller-assignment-map (&key
				  (name :unnamed-controller-assignments)
				  (parent *global-controller-assignments*)
				  (transient t))
  (make-instance 'controller-assignment-map
		 :name name
		 :parent parent
		 :transient transient))

(defmethod assign-controller! ((alias symbol)(value fixnum) &key
			       (cmap *global-controller-assignments*))
  (property! cmap alias value))

(defmethod controller-assignment ((n fixnum) &key (cmap :ignore))
  (logand n #x7F))

(defmethod controller-assignment ((alias symbol) &key (cmap *global-controller-assignments*))
  (let ((ctrl (property cmap alias)))
    (or ctrl
	(error (format nil "~A is not a defined controller" alias)))))
	

(setf *global-controller-assignments* (controller-assignment-map
				       :name :global-controller-assignments
				       :parent nil
				       :transient nil))

(assign-controller! :bank-select-msb 0)
(assign-controller! :bank-select 0)
(assign-controller! :wheel 1)
(assign-controller! :breath 2)
(assign-controller! :foot 4)
(assign-controller! :port-time 5)
(assign-controller! :data-msb 6)
(assign-controller! :volume 7)
(assign-controller! :ballance 8)
(assign-controller! :efx-control-1 12)
(assign-controller! :efx-control-2 13)
(assign-controller! :bank-select-lsb 32)
(assign-controller! :damper-pedal 64)
(assign-controller! :port-pedal 65)
(assign-controller! :sostenuto-pedal 66)
(assign-controller! :soft-pedal 67)
(assign-controller! :legato-switch 68)
(assign-controller! :hold-pedal 69)
(assign-controller! :port-control 84)
(assign-controller! :data-dectrment 96)
(assign-controller! :data-increment 97)
(assign-controller! :nrpn-lsb 98)
(assign-controller! :nrpn-msb 99)
(assign-controller! :rpn-lsb 100)
(assign-controller! :rpn-msb 101)
(assign-controller! :reset-all 121)
(assign-controller! :local-control 122)
(assign-controller! :all-notes-off 123)
(assign-controller! :omni-off 124)
(assign-controller! :omni-on 125)
(assign-controller! :mono-on 126)
(assign-controller! :mono-off 127)
