;;;; org.gripside.cyco.node
;;;;
;;;; Defines NODE class for building graphs.
;;;; Each node has a single parent and 0 or more child nodes.  If the parent
;;;; is nil the node is a root node.
;;;;
;;;; Nodes also have a set of properties in the form of key/value pairs.
;;;; Child nodes inherit property values from their parent but may override
;;;; them. 
;;;;
;;;; NODE
;;;;  |
;;;;  +-- INSTRUMENT
;;;;  +-- CHANNEL-ASSIGNMENT-MAP
;;;;  +-- CONTROLLER-ASSIGNMENT-MAP
;;;;  +-- CHORD-DICTIONARY
;;;;  +-- PROJECT
;;;;  +-- TIMESIG
;;;;  |    |
;;;;  |    +-- SECTION
;;;;  |
;;;;  +-- PART
;;;;       |
;;;;       +-- PROGRAMS
;;;;       +-- FIXED-PART
;;;;       +-- EPART
;;;;       +-- QBALL
;;;;       |    |
;;;;       |    +-- metronome (not actual class)
;;;;       |
;;;;       +-- CONTROLBALL
;;;;            |
;;;;            +-- PRESSUREBALL
;;;;            +-- BENDBALL
;;;;

(in-package :cyco)

(defclass node nil
  ((parent
    :type node
    :accessor parent
    :initform nil
    :initarg :parent)
   (transient				; A transient node is a node
    :type t				; which is not a permanent element
    :reader is-transient		; of a tree.  By default non-transient
    :initform t				; nodes are not removed from the 
    :initarg :transient)		; tree by the prune methods.
   (children				
    :type list
    :accessor children
    :initform '())
   (name
    :type symbol
    :reader name
    :initform :NONAME
    :initarg :name)
   (properties
    :type hash-table
    :reader properties
    :initform (make-hash-table))))

(defmethod node-p ((n node)) n)

(defmethod nameable-p ((n node)) n)

(defmethod root-p ((n node))
  (and (node-p n)
       (not (parent n))))

(defmethod cohorts ((n node))
  "Returns a list of all nodes which are 'sibling' nodes to n."
  (remove n (children (parent n))))

(defmethod has-child? ((n node)(other node))
  "Returns true if other is a child of node n."
  (some #'(lambda (obj)(eq obj other))(children n)))

(defmethod has-child? ((n node)(cname symbol))
  (some #'(lambda (obj)(string= (->string cname)(->string (name obj))))
	(children n)))

(defmethod collect-children ((n node) &key
			     (test #'(lambda (obj)(dismiss obj) t)))
  "Returns list of child nodes for which test is true."
  (let ((acc '()))
    (dolist (c (children n))
      (if (funcall test c)
	  (push c acc)))
    (reverse acc)))

(defmethod get-child ((n node)(name t))
  "Returns first child with matching name.
   Second return values are all other nodes with the same name."
  (let* ((target (->string (name name)))
	 (rs (collect-children n :test #'(lambda (obj)
					   (string= target (->string (name obj)))))))
    (values (car rs) rs)))

(defmethod get-child ((n node)(child node))
  "If child is a child of n, return it. Otherwise return nil."
  (if (has-child? n child)
      child))

(defgeneric find-node (root name &key default))

;; Walk tree from root and return first node encountered with matching name
;; If no matching node found, return default
;;
(defmethod find-node ((root node)(name symbol) &key (default nil))
  "Walk tree starting at root and return list all nodes with with matching 
   name."
  (let* ((clist (children root))
	 (limit (length clist))
	 (i 0)
	 (rs (find name clist :key #'name)))
    (while (and (not rs)(< i limit))
      (setf rs (find-node (nth i clist) name))
      (setf i (1+ i)))
    (or rs default)))

(defmethod find-node ((root node)(child node) &key (default nil))
  (find-node root (name child) :default default))


;; Add child to node if test predicate is true
;; Predicate takes two values (a b) where a is the parent node and c
;; is the child node.
;; The default behavior it to add the child if it is not already a child
;; of the node.
;; Returns child if it was added
;; Returns nil if child was not added.
;;
(defmethod add-child! ((n node)(child node) &key
		       (test #'(lambda (nd ch)(not (has-child? nd ch)))))
  (if (funcall test n child)
      (progn
	(push child (children n))
	(setf (parent child) n)
	child)
    nil))

;; If child is transient or force is true, remove child from tree.
;;
(defmethod prune-leaf! ((n node)(child node) &key (force nil))
  "Remove child from node.
   If child node is transient, do not remove it unless force is true."
  (if (get-child n child)
      (if (or force (is-transient child))
	  (progn
	    (setf (children n)(remove child (children n) :test #'eq))
	    (setf (parent child) nil)
	    child)
	nil)
    nil))

;; Remove child with matching name from tree, but only if it is
;; transient or force is true
;;
(defmethod prune-leaf! ((n node)(name symbol) &key (force nil))
  (prune-leaf! n (get-child n name) :force force))


(defgeneric prune-tree! (n &key force))

;; Tear tree apart.
;; Remove all transient nodes from tree.
;; If force is true remove all nodes.
;; Each sub-tree is also pruned.
;;
(defmethod prune-tree! ((n node) &key (force nil))
  "Disassociate all nodes from this tree.
   If force is nil (the default) only remove transient nodes,
   Otherwise remove all nodes."
  (dolist (c (children n))
    (prune-leaf! n c :force force)
    (prune-tree! c :force force)))

(defun node (name &key (parent nil)(transient t))
  "Creates new NODE object."
  (let ((rs (make-instance 'node :name name :parent parent :transient transient)))
    (add-child! parent rs)
    rs))

(defmethod property ((n null)(key symbol) &key (default :error))
  (if (eq default :error)
      (error (format nil "Property key ~A is not defined" key))
    default))

(defmethod property ((n node)(key symbol) &key (default :error))
  (or (gethash key (properties n))
      (property (parent n) key :default default)))

(defmethod property! ((n node)(key symbol)(value t))
  (setf (gethash key (properties n)) value))

(defmethod property-keys ((n null) &optional (local-only :ignore))
  (dismiss local-only)
  nil)
  
(defmethod property-keys ((n node) &optional (local-only nil))
  (let ((acc '()))
    (maphash #'(lambda (a b)
		 (dismiss b)
		 (push a acc))
	     (properties n))
    (if local-only
	acc
      (progn 
	(dolist (k (property-keys (parent n)))
	  (push k acc))
	(remove-duplicates acc)))))

(defmethod ->string ((n node))
  (format nil "Node '~A'" (name n)))

(defmethod clone ((n node) &key
		  (newname "~A-clone")
		  (parent nil)
		  (hook #'identity))
  (let* ((new-name (format nil newname (name n)))
	 (new-parent (or parent (parent n)))
	 (other (node new-name :parent nil)))
    (dolist (key (property-keys n :local))
      (property! other key (clone (property n key))))
    (dolist (c (children n))
      (clone c :parent other))
    (setf other (funcall hook other))
    (setf (parent other) new-parent)
    (add-child! new-parent other)
    other))

(defmethod dump ((n node) &key (depth 0)(max-depth 10))
  (if (< depth max-depth)
      (progn
	(format t "~A~A ~A~%" (tab depth)(type-of n)(name n))
	(dolist (c (children n))
	  (dump c :depth (1+ depth) :max-depth max-depth))))
  nil)
    
(defmethod remarks! ((obj node)(txt t))
  (if txt
      (property! obj :remarks (->string txt))
    (property! obj :remarks nil)))

(defmethod remarks ((obj node))
  (property obj :remarks :default ""))

(defmethod transposable-p ((n node))
  (property n :transposable :default nil))

(defmethod transposable ((n node)(flag t))
  (property! n :transposable flag))


;;; TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST
;;;
;; (param root (node :root :transient nil))
;; (param animal (node :animal :parent root :transient nil))
;; (param plant (node :plant :parent root :transient nil))
;; (param reptile (node :reptile :parent animal))
;; (param amphibian (node :amphibian :parent animal))
;; (param lizard (node :lizard :parent reptile))
;; (param alligator (node :alligator :parent reptile :transient nil))
;; (param bird (node :bird :parent animal))
;; (param grain (node :grain :parent plant))
;; (param tree (node :tree :parent plant))
;; (param rice (node :rice :parent grain))
;; (param corn (node :corn :parent grain))
;; (param oak (node :oak :parent tree))
;; (param pine (node :pine :parent tree))
;; (print-tree root)
