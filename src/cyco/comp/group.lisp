;;;; cyco.comp.group
;;;;
;;;; A GROUP is a collection of PARTs within a specific SECTION which allows
;;;; related parts to be muted or soloed together.
;;;;

(in-package :cyco)

(defclass group nil
  ((name
    :type symbol
    :reader name
    :initform nil
    :initarg :name)
   (parent-section
    :type section
    :reader parent
    :initform nil
    :initarg :parent)
   (state
    :type symbol			; nil :mute :solo
    :accessor group-state
    :initform nil)
   (members
    :type list
    :accessor members
    :initform '()
    :initarg :members)))

(defmethod nameable-p ((g group)) g)

(defmacro group (name members &key (project *project*)(section nil))
  "Create new GROUP instance and bind it to name.
   name    - Symbol, the group's name.
   members - List of PARTS or PART names.
   :project - Default *PROJECT*
   :section - Default current section of project."
  `(progn
     (validate-part-parents ,project ,section)
     (let* ((sec (or ,section (current-section ,project)))
	    (grp (make-instance 'group :name ',name :parent sec)))
       (setf (gethash ',name (groups sec)) grp)
       (let* ((acc '()))
	 (dolist (i (->list ,members))
	   (cond ((and (part-p i)(has-child? sec i))
		  (push i acc))
		 ((and (symbolp i)(has-child? sec i))
		  (push (get-child sec i) acc))
		 (t (let ((frmt "Can not add ~A to group ~A of section ~A"))
		      (warning (format nil frmt i ,name (name sec)))))))
	 (setf (members grp) acc))
       (param ,name grp)
       grp)))

(defmethod dump ((grp group) &key (depth 0)(max-depth 10))
  (if (< depth max-depth)
      (let ((pad (tab (1+ depth))))
	(format t "~ASection ~A Group ~A   (state ~A)~%"
		(tab depth)(name (parent grp))(name grp)(group-state grp))
	(if (< (1+ depth) max-depth)
	    (dolist (i (members grp))
	      (format t "~A~A~%" pad (name i))))))
  nil)
       
(defmethod cohorts ((grp group))
  "Returns list of all sibling GROUPs to grp within it's parent section."
  (let ((acc '()))
    (maphash #'(lambda (key val)
		 (if (not (eq val grp))
		     (push val acc)))
	     (groups (parent grp)))
    acc))

(defmethod mute ((grp group)(flag t))
  "Set mute status of all PARTs in the GROUP to flag."
  (setf (group-state grp) (and flag :mute))
  (dolist (prt (members grp))
    (mute prt flag)))

(defmethod solo ((grp group))
  "Unmute all PARTs in the GROUP and simultaneously mute all other groups.
   The behavior is not defined if a PART is a member of more then one GROUP."
  (dolist (other (cohorts grp))
    (mute other :mute))
  (dolist (prt (members grp))
    (mute prt nil))
  (setf (group-state grp) :solo))

(defmethod transpose! ((grp group)(x t) &key (range '(0 127)))
  (dolist (e (members grp))
    (transpose! e x :range range))
  grp)

(defmethod invert! ((grp group)(pivot t) &key (range '(0 127)))
  (dolist (e (members grp))
    (invert! e pivot :range range))
  grp)
