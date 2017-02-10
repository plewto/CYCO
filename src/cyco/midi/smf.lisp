;;;; cyco.midi.smf
;;;;
;;;; SMF class defines standard MIDI file.
;;;; Only format type 1 (single track) files are supported.
;;;;

(in-package :cyco)

(defclass smf nil
  ((header
    :type smf-header
    :reader smf-header
    :initform nil
    :initarg :header)
   (tracks
    :type list
    :accessor smf-tracks
    :initform nil
    :initarg :tracks)
   (filename
    :type string
    :accessor filename
    :initform nil
    :initarg :filename)))

(defun smf (&key (format 1)
		 (track-count 1)
		 (initial-tempo 120)
		 (division +TICKS-PER-QUARTER-NOTE+))
  "Create standard MIDI file.
   :format - file format, only type 1 is supported.
   :track-counts - Only single track is supported
   :initial-tempo in BPM, default 120.
   :division - number of ticks per quarter note.  Defaults to 
               +TICKS-PER-QUARTER-NOTE+
   Returns instance of SMF"
  (make-instance 'smf
		 :header (make-instance 'smf-header
					:format format
					:division division)
		 :tracks (let ((acc '()))
			   (dotimes (i track-count)
			     (push (make-instance 'smf-track
						  :name (format nil "Track-~A" i)
						  :initial-tempo initial-tempo)
				   acc))
			   (reverse acc))))
		 
(defmethod ->string ((obj smf))
  (format nil "SMF (format ~A, tracks ~A) file: '~A'"
	  (smf-format (smf-header obj))
	  (length (smf-tracks obj))
	  (filename obj)))

(defmethod track-count ((obj smf))
  (length (smf-tracks obj)))

(defmethod render-smf ((obj smf) &key (pad-end 1.0))
  (let* ((tcount (track-count obj))
	 (acc (render-smf-header (smf-header obj) tcount)))
    (dolist (trk (smf-tracks obj))
      (setf acc (append acc (render-smf-track trk pad-end))))
    acc))

(defmethod save-smf ((obj smf)(filename string) &key
		     (pad-end 1)
		     (no-overwrite nil))
  (format t "Writing standard MIDI file '~A'~%" filename)
  (let ((data (render-smf obj :pad-end pad-end))
	(strm (open filename
		    :direction :output
		    :if-exists (if no-overwrite nil :supersede)
		    :element-type '(unsigned-byte 8))))
    (dolist (b data)
      (write-byte b strm))
    (close strm)
    (setf (filename obj) filename)
    ))
