;;;; CYCO local procussion
;;;;

(in-package :cyco)

(constant +PROCUSSION-PROGRAMS+
          '((ampitheater     . (000 standard)) ; *
            (mega-drums      . (001 standard)) ; *
            (rock-n-roll     . (002 standard)) ; *
            (palladium       . (003 standard)) ; *
            (jazz-drums      . (004 standard)) ; *
            (metal-drums     . (005 standard)) ; *
            (rap-session     . (006 standard)) ; *
            (latin-drums     . (007 ))         ; *       
            (percussion1     . (008 ))         ; *         
            (toolkit         . (009 ))         ; * 
            (ambient-rock    . (010 standard)) ; *
            (acoustic-kit    . (011 standard)) ; *
            (thundadome      . (012 tuned2))   ; *           
            (discoball       . (013 ))         ; *
            (vibrations      . (014 tuned))    ; *               
            (rock-drums      . (015 standard)) ; *
            (house-machine   . (016 standard)) ; *
            (fusion-stix     . (017 standard)) ; *
            (found-sound     . (018 ))         ; *         
            (marimba         . (019 tuned))    ; *               
            (space-drums     . (020 standard)) ; *
            (hard-rock       . (021 standard)) ; *
            (stadium-rox     . (022 standard)) ; *
            (dance-2000      . (023 standard)) ; *
            (industry        . (024 tuned2))   ; *               
            (heavy-metal     . (025 standard)) ; *
            (hip-hop         . (026 standard)) ; *
            (rosewood        . (027 tuned))    ; *               
            (percussion2     . (028 ))         ; *        
            (sound-fx        . (029 ))         ; *         
            (sluggo-drums    . (030 standard)) ; *
            (beatbox         . (031 ))         ; *
            (rocket-drums    . (032 ))         ; *         
            (huge-room       . (033 standard)) ; *
            (churchyard      . (034 tuned2))   ; *           
            (drum-dance      . (035 standard)) ; *
            (percussion3     . (036 ))         ; *         
            (malletbells     . (037 tuned))    ; *               
            (hip-house       . (038 ))         ; * 
            (latin-layers    . (039 ))         ; *         
            (big-band        . (040 not-defined))
            (multi-fx        . (041 ))         ; *         
            (heavyosity      . (042 standard)) ; *
            (ritual-night    . (043 tuned2))   ; *               
            (dance-club      . (044 standard)) ; *
            (indo-steel      . (045 tuned))    ; *               
            (orch-percussion . (046))          ; *
            (country-kit     . (047 standard)) ; *
            (killer          . (048 tuned))    ; *               
            (heavy-hannded   . (049 ))         ; *
            (percussives     . (050 ))         ; *
            (mystic-land     . (051 tuned))    ; *               
            (beach-party     . (052 ))
            (intervallix     . (053 tuned2))   ; *               
            (jazzy-traps     . (054 not-defined))
            (clavarimba      . (055 tuned))    ; *               
            (rockabilly      . (056 standard)) ; *
            (control-snares  . (057 ))         ; *
            (lotsa-kicks     . (058 ))         ; *
            (all-snares      . (059 ))         ; *
            (more-toms       . (060 ))         ; *
            (more-cymbals    . (061 ))         ; *
            (more-percussion . (062 ))         ; *
            (more-basses     . (063 tuned3))   ; *           
	    (all-cymbals     . (064))          ; *
	    (redsky          . (065))          ; *
	    (cymbals2        . (066))          ; *
	    (snare-eater     . (067 not-defined))
	    (proshake        . (068))          ;*
            (metronome       . (127))))    

(defun procussion-program-hook (time cindex program bank)
  (dismiss bank)
  (let* ((a (assoc program +PROCUSSION-PROGRAMS+))
	 (pnum (cond ((eq program :?)
		      (progn
			(format t "Emu Procussion program map~%")
			(dolist (a +PROCUSSION-PROGRAMS+)
			  (format t "    ~14A -> ~3D~%" (car a)(cdr a)))
			nil))
		     ((and (integerp program)(>= program 0)(< program 128))
		      program)
		     (a (car (cdr a)))
		     (t :ERROR))))
    (cond ((eq pnum :ERROR)
	   (let ((frmt "~A is an invalid Procussion program"))
	     (cyco-warning (format nil frmt program))
	     nil))
	  (pnum
	   (list (cons time (midi-program-change cindex pnum))))
	  (t nil))))
					     
(param procussion (create-instrument
		   'PROCUSSION
		   :parent emu
		   :transient nil
		   :program-change-hook #'procussion-program-hook))

(param pro1 (create-instrument
	     'PRO1
	     :parent procussion
	     :transient nil
	     :channel :pro1))

(param pro2 (create-instrument
	     'PRO2
	     :parent procussion
	     :transient nil
	     :channel :pro2))

(param pro3 (create-instrument
	     'pro3
	     :parent procussion
	     :transient nil
	     :channel :pro3))

(load-local "emu/procussion/standard-kit")
(load-local "emu/procussion/latin-drums")
(load-local "emu/procussion/percussion1")
(load-local "emu/procussion/toolkit")
(load-local "emu/procussion/tuned-instruments")              
(load-local "emu/procussion/discoball")
(load-local "emu/procussion/found-sounds")
(load-local "emu/procussion/percussion2")
(load-local "emu/procussion/sound-fx")
(load-local "emu/procussion/beatbox")
(load-local "emu/procussion/rocket-drums")
(load-local "emu/procussion/percussion3")
(load-local "emu/procussion/hip-house")
(load-local "emu/procussion/latin-layers")
(load-local "emu/procussion/multi-fx")
(load-local "emu/procussion/orch-percussion")
(load-local "emu/procussion/heavy-handed")
(load-local "emu/procussion/percussives")
(load-local "emu/procussion/control-snares")
(load-local "emu/procussion/lotsa-kicks")
(load-local "emu/procussion/all-snares")
(load-local "emu/procussion/more-toms")
(load-local "emu/procussion/more-cymbals")
(load-local "emu/procussion/more-percussion")
(load-local "emu/procussion/all-cymbals")
(load-local "emu/procussion/redsky")
(load-local "emu/procussion/cymbals2")
(load-local "emu/procussion/proshake")

(let ((acc (format  nil "Available Procussion Instruments:~%")))
  (dolist (i +PROCUSSION-PROGRAMS+)
    (setf acc (str+ acc (format nil "    [~3d] ~16A ~A~%"
				(second i)(car i)(or (third i) "")))))
  (remarks! procussion acc))
  
