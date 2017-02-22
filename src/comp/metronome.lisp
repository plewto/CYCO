
(in-package :cyco)

(defun --metronome-key-list (bars beats)
  (let ((pat1 (cons 'phrase (copies (1- beats) 'beat)))
	(pat2 (cons 'accent (copies (1- beats) 'beat))))
    (flatten (cons pat1 (copies (1- bars) pat2)))))

(defmacro metronome (name &key
		      (instrument *metronome-instrument*)
		      (project *project*)
		      (section nil))
  "Creates QBALL for use as metronome.
   name       - Symbol, the QBALL is bound to name.
   instrument - Default *METRONOME-INSTRUMENT*
   project    - Default *PROJECT*
   section    - Default current section of project.

   The instrument keynumber-map should accept the following symbols:
   BEEP ACCENT and PHRASE.  The PHRASE tone is produced each time 
   the phrase is repeated.  The ACCENT tone is produced at the start
   of each bar.  The BEEP tone produced at all other beats."
  `(progn
     (validate-part-parents ,project ,section)
     (let* ((sec (or ,section (current-section ,project)))
     	    (bars (bar-count sec))
     	    (beats (beat-count sec))
     	    (cue-list (let ((acc '()))
     	    		(dotimes (br bars)
     	    		  (dotimes (bt beats)
     	    		    (push (list (1+ br)(1+ bt) 1) acc)))
     	    		(reverse acc)))
     	    (key-list (--metronome-key-list bars beats))
     	    (amp-list key-list)
     	    (prt (make-instance 'qball
     	    			:name ',name
     	    			:instruments (->list ,instrument)
     	    			:period (duration sec)
     	    			:cue (cycle :of cue-list)
     	    			:key (cycle :of key-list)
     	    			:dur (cycle :of '(0.01))
     	    			:amp (cycle :of amp-list))))
       (property! prt :qfn #'bar)
       (property! prt :keynumber-map #'identity)
       (property! prt :duration-map #'identity)
       (property! prt :amplitude-map #'identity)
       (add-child! sec prt)
       (property! prt :qfn #'bar)
       (param ,name prt)
       prt)))
