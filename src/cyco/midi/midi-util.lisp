;;;; cyco.midi.midi-util
;;;;


(in-package :cyco)


;;; ************************************************************************
;;;                     MIDI variable length values (vlv)
;;;                                      
;;; A variable length value uses the low order 7 bits of a byte to
;;; represent the value or part of the value. The high order bit is an
;;; "escape" or "continuation" bit. All but the last byte of a variable
;;; length value have the high order bit set. The last byte has the high
;;; order bit cleared. The bytes always appear most significant byte first.
;;; 

;; Converts fixnum to list of vlv bytes.
;;
(defun fixnum->midi-vlv (value)
  (let ((acc (list (logand value #x7f)))
	(n (ash value -7)))
    (while (plusp n)
      (push (+ #x80 (logand n #x7f)) acc)
      (setf n (ash n -7)))
    acc))

;; Converts list of vlv bytes to fixnum.
;;
(defun midi-vlv->fixnum (mbv)
  (let ((acc 0)
	(scale 1))
    (dolist (byte (reverse mbv))
      (setf acc (+ acc (* scale (logand byte #x7f))))
      (setf scale (* scale 128)))
    acc))

;; Read vlv bytes from array and convert to fixnum.
;; ARGS:
;;   ary - array
;;   offset - index into array where vlv begins
;; RETURNS:
;;    cons  (value . new-offset)
;;          value      - value of vlv
;;          new-offset - location of first byte after vlv
;;
(defun read-midi-vlv (ary offset)
  (let* ((byte (aref ary offset))
	 (acc (list byte)))
    (while (plusp (logand byte #x80))
      (setf offset (1+ offset))
      (setf byte (aref ary offset))
      (push byte acc))
    (cons (midi-vlv->fixnum (reverse acc)) (1+ offset))))
    
;;; ********************************************************************** 
;;;			     MIDI Bend values
;;;
;;; MIDI bend is 14-bits specified as lower 7-bits of two bytes.
;;; The high bits are always cleared.  The byte order is lsb msb.
;;;

;; Convert normalized bend value with range (-1..+1) to
;; bend message data bytes
;; ARGS:
;;   bnd - float (-1..+1)
;; RETURNS:
;;   Two element vector #(lsb msb)
;;
(defun bend->midi-data (bnd)
  (let* ((v (truncate (+ (* (min bnd 0.9999) 8192) 8192)))
	 (lsb (logand v #x7f))
	 (msb (logand (ash v -7) #x7f)))
    (vector lsb msb)))


;; Converts 14-bit MIDI bend data to normalized (-1..+1)
;;
(defun midi-data->bend (lsb msb)
  (let* ((b14 (+ (ash msb 7) lsb))
	 (bend (- (* 1/8192 b14) 1.0))
	 (scale 1000.0))
    (/ (round (* bend scale)) scale)))

;; Read 2 data bytes from array and convert to normalized bend.
;; ARGS:
;;   ary - The array
;;   offset - location in ary to read
;; RETURNS: cons (bnd . new-offset)  
;;      bnd - normalized bend, float (-1..+1)
;;      new-offset - first location after bend bytes.
;;                   new-offset is always be offset+2
;;
(defun read-midi-bend (ary offset)
  (let* ((lsb (aref ary offset))
	 (msb (aref ary (1+ offset)))
	 (bend (midi-data->bend lsb msb)))
    (cons bend (+ 2 offset))))

;;; ********************************************************************** 
;;;			     MIDI Data Values
;;;
;;; MIDI data uses lower 7-bits of a byte.  The high bit is always clear
;;;


;; Converts normalized value with range (0..1) to MIDI data byte
;;
(defun norm->midi-data (n)
  (let ((v (limit (truncate (* n 127)) 0 127)))
    v))

(defun midi-data->norm (d)
  (let ((scale 1000.0)
	(norm (/ d 127.0)))
    (/ (round (* norm scale)) scale)))

;; Reads single un-singed midi data byte from array
;; ARGS:
;;    ary - array
;;    offset - location in array to read
;; RETURNS:  cons (n . new-offset)
;;    n - normalized value (0..1)
;;    new-offset - location in array after byte.
;;                 new-offset is always offset+1
(defun read-midi-data (ary offset)
  (let ((byte (aref ary offset))
	(scale 10000.0))
    (cons (/ (round (* (/ byte 127.0) scale)) scale)
	  (1+ offset))))

;; Converts signed normalized value (-1..+1) to single
;; MIDI byte (0..127)
;;
(defun signed-norm->midi-data (n)
  (let ((v (+ (* 127/2 n) 127/2)))
    (limit (truncate v) 0 127)))

;; Reads signed MIDI byte from array
;; ARGS:
;;    ary - array
;;    offset - location in array to read.
;; RETURNS: cons (n . new-offset)
;;    n - normalized value (-1..+1)
;;    new-offset = offset+1
;;
(defun read-signed-midi-data (ary offset)
  (let ((byte (aref ary offset))
	(scale 10000.0))
    (cons (/ (round (* (- (* 2/127 byte) 1.0) scale)) scale)
	  (1+ offset))))
	 

;;; ********************************************************************** 
;;;		    MIDI Tempo As MicroSeconds Per Beat
;;;

(defun bpm->beat-period (bpm)
  (/ 60 bpm))

(defun bpm->microseconds (bpm)
  (truncate (* 1e6 (bpm->beat-period bpm))))
	     
(defmethod tick-duration ((bpm number) &optional (unit 'q))
  (let* ((scale (cond ((eq unit 'q) 1.0)
		      ((eq unit 'w) 4.0)
		      ((eq unit 'h) 2.0)
		      ((eq unit 'e) 0.5)
		      ((eq unit 's) 0.25)
		      (t 1.0)))
	(period (/ (* 60.0 scale) bpm)))
    (/ period +TICKS-PER-QUARTER-NOTE+)))
