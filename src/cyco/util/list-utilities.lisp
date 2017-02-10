;;;; cyco.util.list-utilities
;;;;

(in-package :cyco)

;; (defmethod clone ((c cons) &key parent newname (hook #'identity))
;;   (funcall hook (cons (clone (car c))(clone (cdr c)))))

(defmethod clone ((lst list) &key parent newname (hook #'identity))
  (let ((acc '()))
    (dolist (e lst)
      (push (clone e) acc))
    (funcall hook (reverse acc))))

(defmethod ->list ((obj list)) obj)

(defmethod ->list ((obj vector))
  (coerce obj 'list))

(defmethod ->list ((obj string))
  (list obj))

(defmethod ->list ((obj t))
  (list obj))

(defmethod final ((lst list))
  (car (reverse lst)))

(defmethod butfinal ((lst list))
  (butlast lst))

(defmacro push-end (obj lst)
  "Push obj to end of list lst.
   (push-end 'foo '(A B C))  --> (A B C FOO)"
  `(setf ,lst (append ,lst (list ,obj))))

(defun assocv (key alst &optional (default nil))
   "(assocv key alst &optional (default nil))
    Return value of key from assoc-list alst. 
    If alst does not have matching key return default"
  (or (cdr (assoc key alst)) default))

(defmacro apush (key value alst)
   "Push key/value pair to front of alist."
  `(setf ,alst (cons (cons ,key ,value) ,alst)))

(defmacro apush-new (key value alst)
   "Push key/value pair to alist iff alst does not already contain key.
    If alst contains key update value of key to value."
  `(if (not (assoc ,key ,alst))
       (apush ,key ,value ,alst)
     (progn 
       (setf (cdr (assoc ,key ,alst)) ,value)
       ,alst)))

(defun copies (n &optional (item nil))
   "Return list of n copies of item."
  (if (plusp n)
      (cons item (copies (1- n) item))
    nil))

(defun --range- (start end delta)
  (let ((acc '())
	(value start))
    (while (>= value end)
      (push value acc)
      (setf value (+ value delta)))
    (reverse acc)))

(defun --range+ (start end delta)
  (let ((acc '())
	(value start))
    (while (<= value end)
      (push value acc)
      (setf value (+ value delta)))
    (reverse acc)))

(defun range (start end &key (by 1))
  "Return arithmetic sequence."
  (if (> start end)
      (--range- start end (- (abs by)))
    (--range+ start end (abs by))))

(defun cnth (n lst)
   "Return nth element from list in circular manner."
  (nth (rem (abs n) (length lst)) lst))

;; (defun flatten (lst)
;;   "Remove all list nesting retuning a flat list"
;;   (cond ((null lst) nil)
;; 	((atom lst)(list lst))
;; 	(t (append (flatten (car lst))
;; 		   (flatten (cdr lst))))))

;; Non-recursive flatten from rosettacode
;; https://rosettacode.org/wiki/Flatten_a_list#Common_Lisp
;;
(defun flatten (obj)
  (do* ((result (list obj))
        (node result))
       ((null node) (delete nil result))
    (cond ((consp (car node))
           (when (cdar node) (push (cdar node) (cdr node)))
           (setf (car node) (caar node)))
          (t (setf node (cdr node))))))

(defun flatten1 (lst)
  "Remove one level of list nesting and return"
  (let ((acc '()))
    (dolist (e lst)
      (cond ((atom e)
	     (setf acc (append acc (list e))))
	    (t (setf acc (append acc e)))))
    acc))

(defun even-elements (lst)
  "Return only even numbered elements from list"
  (if lst
      (cons (car lst)
	    (even-elements (cdr (cdr lst))))
    nil))

(defun odd-elements (lst)
  "Return only odd numbered elements from list"
  (even-elements (cdr lst)))

;; Partition list into n mutually exclusive sub-list

;; Each element e of list appears in one, and only one, of the resulting
;; sub-list based on it's position within lst.
;; sub-list 0 contains elements 0 n 2n 3n ... of lst
;; sub-list 1 contains elements 1 n+1 2n+1 3n+1 of lst etc
;;
;; see partition in filter.lisp
;;
(defun npartition (lst n)
  (let ((acc '()))
    (dotimes (m n)(push '() acc))
    (do ((i 0 (1+ i))
	 (j 0 (rem (1+ j) n)))
	((= i (length lst))
	 (do ((k 0 (1+ k)))
	     ((= k (length acc)))
	   (setf (nth k acc)
		 (reverse (nth k acc))))
	 acc)
      (let ((sub (nth j acc)))
	(setf sub (cons (nth i lst) sub))
	(setf (nth j acc) sub)))))

;; Rotate right 1 step
;;
(defun --rotate+1 (lst &optional (fn #'identity))
  (append (cdr lst)(list (funcall fn (car lst)))))

;; Rotate left 1 step
;;
(defun --rotate-1 (lst &optional (fn #'identity))
  (cons (funcall fn (car (last lst)))
	(butlast lst)))

;; Rotate n steps right (n>0) or left (m<0) or zero (n==0)
;; n = 'rnd --> rotate random times
;;
(defun rotate (lst &optional (n 1) (fn #'identity)(state *random-state*))
  (cond 
   ((eq n 'rnd)
    (setf lst (rotate lst (random (length lst) state) fn)))
   ((plusp n)
    (dotimes (i n)
      (setf lst (--rotate+1 lst fn))))
   ((minusp n)
    (dotimes (i (abs n))
      (setf lst (--rotate-1 lst fn))))
   (t nil))
  lst)

;; Return random permutation of list
;;
(defun permutation (lst &optional (state *random-state*))
  (let ((acc '()))
    (while lst
	   (setf lst (rotate lst 'rnd #'identity state))
	   (push (car lst) acc)
	   (setf lst (cdr lst)))
    acc))

(defun zip (a b)
  (do ((acc '())
       (i 0 (1+ i)))
      ((= i (max (length a)(length b)))
       (reverse acc))
    (if (< i (length a))
	(push (nth i a) acc))
    (if (< i (length b))
	(push (nth i b) acc))))

;; Sets missing or nil values in lst to corresponding values in template
;; (fill-list '() '(a b c)) --> (a b c)
;; (fill-list '(a nil c) '(0 1 2) --> (a 1 c)
;; (fill-list '(a nil c d) '(0 1 2) --> (a 1 c d)
;;
(defun fill-list (lst template)
  (let ((acc (copies (max (length lst)(length template)))))
    (dotimes (i (length acc))
      (setf (nth i acc)
	    (or (nth i lst)
		(nth i template))))
    acc))

(defmethod pick ((lst list) &key (n 1)(random-state *random-state*))
  (flet ((p ()(nth (random (length lst) random-state) lst)))
    (if (= n 1)
	(p)
      (let ((acc '()))
	(dotimes (i n)
	  (push (p) acc))
	acc))))

;; (defun --list-palindrome-no-elide (lst)
;;   (let* ((a (clone lst))
;; 	 (b (reverse a)))
;;     (append a b)))

;; (defun --list-palindrome-elide-both (lst)
;;   (let* ((a (butlast (cdr lst)))
;; 	 (b (reverse a)))
;;     (append a b)))

;; (defun --list-palindrome-elide-first (lst)
;;   (let* ((a (clone lst))
;; 	 (b (butlast (reverse a))))
;;     (append a b)))

;; (defun --list-palindrome-elide-last (lst)
;;   (let* ((a (clone lst))
;; 	 (b (cdr (reverse a))))
;;     (append a b)))

;; (defmethod palindrome (lst &key (elide nil))
;;   (cond ((eq elide :last)
;; 	 (--list-palindrome-elide-last lst))
;; 	((eq elide :first)
;; 	 (--list-palindrome-elide-first lst))
;; 	(elide (--list-palindrome-elide-both lst))
;; 	(t (--list-palindrome-no-elide lst))))


(defmethod filter ((lst list) &key (test #'true))
  (->list (filter (->vector lst) :test test)))

(defmethod retrograde ((lst list))
  (reverse lst))

(defmethod retrograde! ((lst list))
  (let ((acc (reverse lst)))
    (dotimes (i (length lst))
      (setf (nth i lst)(nth i acc)))
    lst))
	
  
;;; ---------------------------------------------------------------------- 
;;;				  Vectors

(defmethod clone ((vec vector) &key newname parent (hook #'identity))
  (let* ((len (length vec))
	 (dst (make-array len)))
    (dotimes (i len)
      (setf (aref dst i)(clone (aref vec i))))
    (funcall hook dst)))

(defmethod ->vector ((obj vector)) obj)

(defmethod ->vector ((lst list))
  (coerce lst 'vector))

(defmethod ->vector ((obj t))
  (vector obj))

(defmethod pick ((ary vector) &key (n 1)(random-state *random-state*))
  (flet ((p ()(aref ary (random (length ary) random-state))))
    (if (= n 1)
	(p)
      (let ((acc '()))
	(dotimes (i n)
	  (push (p) acc))
	acc))))
		 
(defmethod final ((ary vector))
  (let ((n (length ary)))
    (if (plusp n)
	(aref ary (1- n))
      nil)))

(defmethod butfinal ((ary vector))
  (let ((count (1- (length ary))))
    (if (plusp count)
	(let ((acc (make-array count))) 
	  (dotimes (i count)
	    (setf (aref acc i)(aref ary i)))
	  acc)
      #())))

;; (defmethod palindrome ((ary vector) &key elide)
;;   (->vector (palindrome (->list ary) :elide elide)))

(defmethod filter ((vec vector) &key (test #'true))
  (let ((acc '()))
    (dotimes (i (length vec))
      (let ((e (aref vec i)))
	(if (funcall test e)
	    (push e acc))))
    (->vector (reverse acc))))

