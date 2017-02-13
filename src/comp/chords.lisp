;;;; cyco.comp.chords
;;;;
;;;; Defines CHORD-DICTIONARY class which maps symbolic names to list
;;;; of keynumbers.  CHORD-DICTIONARY extends NODE so that one instance
;;;; may serve as default values for another.   Each project receives a
;;;; copy of *GLOBAL-CHORD-DICTIONARY* and may add local modifications.
;;;;
;;;; TODO: Define more chord types.
;;;;

(in-package :cyco)

(defclass chord-dictionary (node) nil)

(setf *global-chord-dictionary*
      (let ((cd (make-instance 'chord-dictionary
			       :name 'global-chord-dictionary)))
	(flet ((chord! (name template)
		       (property! cd name template)))
	  (chord! '[maj]    '(0 4 7))
	  (chord! '[maj7]   '(0 4 7 11))
	  (chord! '[dom7]   '(0 4 7 10))
	  (chord! '[maj9]   '(0 4 7 11 14))
	  (chord! '[aug]    '(0 4 8))
	  (chord! '[min]    '(0 3 7))
	  (chord! '[min7]   '(0 3 7 10))
	  (chord! '[min9]   '(0 3 7 10 14))
	  (chord! '[dim]    '(0 3 6))
	  cd)))

(defun chord-dictionary (&optional (parent *global-chord-dictionary*))
  "Creates new instance of CHORD-DICTIONARY with 
   *GLOBAL-CHORD-DICTIONARY* as the default parent."
  (make-instance 'chord-dictionary
		 :parent parent
		 :name 'local-chord-dictionary))

(defun parse-chord (root cname &key
			 (project *project*)
			 (inversion 0)
			 (octave nil)
			 (copies nil))
  "Construct chord as list of keynumbers from root and chord name cname.
   root - keynumber, the root key.
   cname - symbol the chord name, or a list of intervals.
   :project - The project suppling the CHORD-DICTIONARY, defaults to *PROJECT*.
   :inversion - fixnum, apply inversion to note list by rotation. default 0.
   :octave - boolean, if true add octave transposed copy of first chord note
             to end of list.  Octave is applied after inversion. Default nil.
   :copies - fixnum or list.  Treat each number in the copies list as a 
             transposition of the original values.   For instance if the 
             original chord is [maj] (0 4 7) and copies is 12, the result 
             is (0 4 7 12 16 19).   If copies is (12 13) the result is
             (0 4 7 12 16 19 13 17 20).
   Returns list."
  (let* ((cd (property project :chord-dictionary))
	 (template (if (listp cname)
		       cname
		     (or (property cd cname :default nil)
			 (error
			  (format nil "~A is not a chord type!" cname))))))
    (setf template (rotate template inversion))
    (if octave (push-end (+ 12 (car template)) template))
    (let ((acc (clone template)))
      (dolist (x (->list copies))
	(setf acc (append acc (transpose template x))))
      (transpose acc (keynumber root)))))

(defun chord! (name template &key (project nil))
  "Define new chord in current project.  By convention chords names are
   between square brackets  [name].  Template should be a list of 
   keynumbers starting in the 0th octave."
  (let* ((cd (property (or project *project*) :chord-dictionary)))
    (property! cd name template)))

