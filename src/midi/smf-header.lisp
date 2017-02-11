;;;; cyco.midi.smf-header
;;;;
;;;; SMF-HEADER class defines header chunk for standard MIDI file.


(in-package :cyco)

(defgeneric render-smf-header (header track-count))
(defgeneric dump-smf-header (header &optional offset))

(defclass smf-header nil
  ((format
    :type fixnum
    :reader smf-format
    :initform 1
    :initarg :format)
   (division
    :type fixnum
    :reader smf-division
    :initform +TICKS-PER-QUARTER-NOTE+
    :initarg :division)))

(defmethod render-smf-header ((header smf-header)(track-count fixnum))
  (flet ((msb (n)(logand (ash n -8) #xFF))
	 (lsb (n)(logand n #xFF)))
    (list (char-code #\M)
	  (char-code #\T)
	  (char-code #\h)
	  (char-code #\d)
	  0 0 0 6
	  (msb (smf-format header))
	  (lsb (smf-format header))
	  (msb track-count)
	  (lsb track-count)
	  (msb (smf-division header))
	  (lsb (smf-division header)))))

(defmethod dump-smf-header ((head smf-header) &optional (offset :ignore))
  (dismiss offset)
  (let ((pad (tab 1)))
    (format t "SMF-HEADER~%")
    (format t "~AId          : 'MThd' ~%" pad)
    (format t "~ALength      :  6~%" pad)
    (format t "~AFormat      :  ~A~%" pad (smf-format head))
    (format t "~ATrack Count :  ?~%" pad)
    (format t "~ADivision    : ~A~%" pad (smf-division head))))

(defmethod dump-smf-header ((ary vector) &optional (offset 0))
  (let ((pad (tab 1)))
    (flet ((item (index label bcount)
		 (format t "~A[~4X] ~8A : " pad (+ offset index) label)
		 (dotimes (i bcount)
		   (format t "~2X " (aref ary (+ offset index i))))
		 (format t "~%")))
      (format t "SMF-HEADER~%")
      (item  0 "ID" 4)
      (item  4 "BCOUNT" 4)
      (item  8 "FRMT" 2)
      (item 10 "TCOUNT" 2)
      (item 12 "DIV" 2))))
	    
(defmethod dump-smf-header ((lst list) &optional (offset 0))
  (dump-smf-header (->vector lst) offset))
