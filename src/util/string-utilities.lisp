;;;; cyco.util.string-utilities
;;;;

(in-package :cyco)

;; (defun tab (&optional (n 1))
;;   "Return string of nx4 spaces."
;;   (let ((frmt (format nil "~~~DA" (* 4 n))))
;;     (format nil frmt "")))

(defmethod ->string ((obj string))
  obj)

(defmethod clone ((obj string) &key newname parent (hook #'identity))
  (dismiss newname parent)
  (funcall hook (format nil "~A" obj)))

(defun str+ (&rest args)
  "Concatenate strings."
  (let ((sargs (mapcar #'->string args)))
    (apply #'concatenate (cons 'string sargs))))

;; (defun last ((str string))
;;   (char (reverse str)))

;; ISSUE: Kind of a mess but it works.
;; Split string by delim character
;; Returns list of substrings
;; All leading/trailing delim characters are trimmed.
(defun split-string (str &optional (delim #\space))
  "Split string by dliminator.
   str   - The string
   delim - Delineation character, defaults to space.
   Returns list."
  (let ((acc '())
	(scc ""))
    (map 'string #'(lambda (c)
		     (if (and (char= c delim)(not (string= (string-trim " " scc) "")))
			 (progn
			   (push (string-trim (->string delim) scc) acc)
			   (setf scc ""))
		       (setf scc (str+ scc c)))
		     c)
	 (string-trim (->string delim) str))
    (push (string-trim (->string delim) scc) acc)
    (setf acc (reverse acc))
    (if (equal acc '("")) nil acc)))

(defun string-starts-with (target str)
  "Returns true if string starts with target."
  (let ((rs (search target str)))
    (eq rs 0)))

(defmethod final ((s string))
  (if (plusp (length s))
      (char (reverse s) 0)
    ""))

(defmethod butfinal ((s string))
  (let ((count (length s)))
    (if (> count 1)
	(subseq s 0 (1- count))
      "")))

(defun scopies (n &optional (char #\.))
  "Returns string which is n copies of char."
  (let ((acc ""))
    (dotimes (i (truncate n))
      (setf acc (str+ acc char)))
    acc))

(defun center-string (s &key (width 75)(prefix ""))
  "Returns new string with s centered."
  (let* ((w (- width (length prefix)))
	 (a (length s))
	 (pad (scopies (/ (- w a) 2) #\space)))
    (str+ prefix pad s)))

