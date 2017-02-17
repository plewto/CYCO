;;;; comp.qlist
;;;;

(in-package :cyco)

(defun cue-list (timesig &key
			 (use-tsubbeats nil)
			 (bars #'(lambda (br) t))
			 (beats #'(lambda (br bt) t))
			 (subbeats #'(lambda (br bt su) t)))
  "Creates time cue-list. 
   Each possible combination of bar/beat/(t)subbeat is tested against the 
   three predicate functions bars, beats and subbeats.  If all three 
   functions return true, then the cue-point is included in the result.

   ARGS:
     timesig        - instance of TIMESIG
     :use-tsubbeats - Bool, if true use timsig tsubebeat count, 
                      otherwise use subbeat count.
     :bars          - Function (lambda (br)) -> bool.
                      Function is called for each possible bar in timesig.
                      If result is nil, the bar is skipped.
     :beats         - Function (lambda (br bt)) -> bool.
                      Function called on each possible combination of 
                      bar br and beat bt in timesig.  If the result is nil
                      the beat is skipped.
     :subbeats      - Function (lambda (br bt su)) -> bool.
                      Function called on each possible combination of
                      bar br, beat bt and (t)subbeat su.  If the result is nil
                      the subbeat is skipped.
  The default behavior is for the three predicates to allow all possible combinations.
  
  RETURNS:  Nested list of form ((br bt su)(br bt su)...) suitable as the cue-list
  of the QBALL, PRESSUREBALL, CONTROLBALL and BENDBALL functions."
  (let* ((acc '())
	 (br-max (bar-count timesig))
	 (br-list (range 1 br-max))
	 (bt-max (beat-count timesig))
	 (bt-list (range 1 bt-max))
	 (sub-max (if use-tsubbeats
		      (tsubbeat-count timesig)
		    (subbeat-count timesig)))
	 (sub-list (range 1 sub-max)))
    (dolist (br br-list)
      (if (funcall bars br)
	  (dolist (bt bt-list)
	    (if (funcall beats br bt)
		(dolist (su sub-list)
		  (if (funcall subbeats br bt su)
		      (push (list br bt su) acc)))))))
    (reverse acc)))
