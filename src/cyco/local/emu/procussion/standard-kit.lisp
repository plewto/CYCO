;;;; cyco local emu procussion standard-ket
;;;;
;;;; Instruments
;;;;   Standard-kit
;;;;       pkick
;;;;       psnare
;;;;       ptom
;;;;       phat
;;;;       pcym
;;;;       ptuned
;;;;
;;;; Programs
;;;;    ampitheater
;;;;    mega-drums
;;;;    rock-n-roll
;;;;    palladium
;;;;    jazz-drums
;;;;    metal-drums
;;;;    rap-session
;;;;    ambient-rock
;;;;    acoustic-kit
;;;;    rock-drums
;;;;    house-machine
;;;;    fusion-stix
;;;;    space-drums
;;;;    hard-rock
;;;;    stadium-rox
;;;;    dance-2000
;;;;    heavy-metal
;;;;    hip-hop
;;;;    sluggo-drums
;;;;    huge-room
;;;;    drum-dance
;;;;    heavyosity
;;;;    dance-club
;;;;    country-kit
;;;;    rockabilly
;;;;
;;;;  key   zone 
;;;;  033 - Z01  Main Kick (Tuned) 
;;;;  034 - Z01  Main Kick (Tuned) 
;;;;  035 - ZO1  Main Kick (Tuned) 
;;;;  036 - ZO1  Main Kick (Tuned) 
;;;;  037 - Z04  Aux Snare 1
;;;;  038 - Z03  Main Snare
;;;;  039 - Z05  Aux Snare 2
;;;;  040 - Z19  Tom (Tuned)
;;;;  041 - Z11  Tom (Tuned)
;;;;  042 - Z07  HH Closed
;;;;  043 - Z12  Tom (Tuned)
;;;;  044 - Z08  HH Open 
;;;;  045 - Z19  Tom (Tuned)
;;;;  046 - Z10  Percussion
;;;;  047 - Z19  Tom (Tuned)
;;;;  048 - Z15  Choke Cymbol (or other percussion)
;;;;  049 - Z16  Cymbal 1 (Crash or ride)
;;;;  050 - Z17  Aux Ride
;;;;  051 - Z18  Cymbal 2 (Crash or Ride)
;;;;  052 - Z09  HH Stack (Controller A)
;;;;  053 - Z02  Kick (Nontransposed)
;;;;  054 - Z02  Kick (Nontransposed)
;;;;  055 - Z02  Kick (Nontransposed)
;;;;  056 - Z02  Kick (Nontransposed)
;;;;  057 - Z02  Kick (Nontransposed)
;;;;  058 - Z02  Kick (Nontransposed)
;;;;  059 - Z06  Snare (Nontransposed)
;;;;  060 - Z06  Snare (Nontransposed)
;;;;  061 - Z06  Snare (Nontransposed)
;;;;  062 - Z06  Snare (Nontransposed)
;;;;  063 - Z06  Snare (Nontransposed)
;;;;  064 - Z06  Snare (Nontransposed)
;;;;  065 - Z21  HH Stomp
;;;;  066 - Z21  HH Stomp
;;;;  067 - Z22  HH Shut
;;;;  068 - Z23  HH 2/3 Open
;;;;  069 - Z23  HH 2/3 Open
;;;;  070 - Z24  HH Open
;;;;  071 - Z24  HH Open
;;;;  072 - Z14  Toned Tom Bass Or Other 
;;;;  073 - Z14  Toned Tom Bass Or Other 
;;;;  074 - Z14  Toned Tom Bass Or Other
;;;;  075 - Z14  Toned Tom Bass Or Other
;;;;  076 - Z14  Toned Tom Bass Or Other
;;;;  077 - Z14  Toned Tom Bass Or Other
;;;;  078 - Z14  Toned Tom Bass Or Other
;;;;  079 - Z14  Toned Tom Bass Or Other
;;;;  080 - Z14  Toned Tom Bass Or Other
;;;;  081 - Z14  Toned Tom Bass Or Other
;;;;  082 - Z14  Toned Tom Bass Or Other
;;;;  083 - Z14  Toned Tom Bass Or Other
;;;;  084 - Z14  Toned Tom Bass Or Other
;;;;  085 - Z14  Toned Tom Bass Or Other
;;;;  086 - Z14  Toned Tom Bass Or Other
;;;;  087 - Z14  Toned Tom Bass Or Other
;;;;  088 - Z14  Toned Tom Bass Or Other
;;;;  089 - Z14  Toned Tom Bass Or Other
;;;;  090 - Z14  Toned Tom Bass Or Other
;;;;  091 - Z14  Toned Tom Bass Or Other
;;;;  092 - Z14  Toned Tom Bass Or Other
;;;;  093 - Z14  Toned Tom Bass Or Other
;;;;  094 - Z14  Toned Tom Bass Or Other
;;;;  095 - Z14  Toned Tom Bass Or Other
;;;;  096 - Z14  Toned Tom Bass Or Other
;;;;  097 - Z14  Toned Tom Bass Or Other
;;;;  098 - Z14  Toned Tom Bass Or Other


(in-package :cyco)

(defkeymap --pro-sk-kick-map '((A . 34)
			       (B . 33)
			       (C . 35)
			       (D . 36)))

(defkeymap --pro-sk-snare-map '((A      . (38 "Main Snare"))
				(B      . (37 "Alternate Snare 1"))
				(C      . (39 "Alternate Snare 2"))
				(D      . (46 "Percussion"))
				(X      . (38 "MOR2 Compatibility --> A"))
				(RIM    . (46 "MOR2 Compatibility --> D")) 
				(CRACK  . (46 "MOR2 Compatibility --> D"))
				(EDGE   . (37 "MOR2 Compatibility --> B"))
				(BOUNCE . (39 "MOR2 Compatibility --> C"))
				(FLAM   . (39 "MOR2 Compatibility --> C"))
				(ROLL   . (39 "MOR2 Compatibility --> C"))
				(X2     . (38 "MOR2 Compatibility --> A"))
				(RIM2   . (46 "MOR2 Compatibility --> D"))
				(CRACK2 . (46 "MOR2 Compatibility --> D"))
				(EDGE2  . (37 "MOR2 Compatibility --> B"))))

(defkeymap --pro-sk-tom-map '((A . (40 "Low tom"))
			      (B . 41)
			      (C . 43)
			      (D . 45)
			      (E . (47 "High tom"))
			      (F . (47 "MOR2 Compatibility --> E"))
			      (A-FLAM . (40 "MOR2 Compatibility"))
			      (B-FLAM . (41 "MOR2 Compatibility"))
			      (C-FLAM . (43 "MOR2 Compatibility"))
			      (D-FLAM . (45 "MOR2 Compatibility"))
			      (E-FLAM . (47 "MOR2 Compatibility"))
			      (F-FLAM . (47 "MOR2 Compatibility"))
			      (A-BOUNCE . (40 "MOR2 Compatibility"))
			      (B-BOUNCE . (41 "MOR2 Compatibility"))
			      (C-BOUNCE . (43 "MOR2 Compatibility"))
			      (D-BOUNCE . (45 "MOR2 Compatibility"))
			      (E-BOUNCE . (47 "MOR2 Compatibility"))
			      (F-BOUNCE . (47 "MOR2 Compatibility"))))

(defkeymap --pro-sk-hat-map '((X       . 42)
			      (OPEN    . 44)
			      (STOMP   . 65)
			      (SHUT    . 67)
			      (OPN     . (68 "2/3 open"))
			      (STACK   . (52 "Mod wheel controlled?"))
			      (STOMP2  . 66)
			      (OPN2    . 69) 
			      (OPEN2   . 70)
			      (OPEN3   . 71)))

(defkeymap --pro-sk-cymbal-map '((RIDE  . 50)
				 (A     . 49)
				 (B     . 51)
				 (CHOKE . 48)))

(defun --pro-sk-tuned-map (kn)
  (if (eq k :?)
      (progn 
	(format t "Standard Procussion kit tuned instrument (72..98)~%")
	72)
    (let* ((range (- 98 72))
	   (k (rem (keynumber kn) range)))
      (+ 72 k))))


(defmacro standard-procussion-kit (kit-name &key 
					    (parent pro1)
					    (kick-name 'pkick)
					    (snare-name 'psnare)
					    (tom-name 'ptom)
					    (hat-name 'phat)
					    (cymbal-name 'pcym)
					    (tuned-name 'ptuned))
  
  `(let* ((program-number (car (cdr (assoc ,kit-name +PROCUSSION-PROGRAMS+))))
	 (trap-kit (create-instrument 
		    ',kit-name
		    :parent ,parent
		    :transient t
		    :program-change-hook
		    #'(lambda (time cindex _ __)
			(list (cons time (midi-program-change cindex program-number))))))
	 (kd (create-instrument ,kick-name
				:parent trap-kit
				:transient t
				:keynumber-map #'--pro-sk-kick-map))
	 (sd (create-instrument ,snare-name
				:parent trap-kit
				:transient t
				:keynumber-map #'--pro-sk-snare-map))
	 (td (create-instrument ,tom-name
				:parent trap-kit
				:transient t
				:keynumber-map #'--pro-sk-tom-map))
	 (hhat (create-instrument ,hat-name
				  :parent trap-kit
				  :transient t
				  :keynumber-map #'--pro-sk-hat-map))
	 (cym (create-instrument ,cymbal-name
				 :parent trap-kit
				 :transient t
				 :keynumber-map #'--pro-sk-cymbal-map))
	 (tuned (create-instrument ,tuned-name
				   :parent trap-kit
				   :transient t
				   :keynumber-map #'--pro-sk-tuned-map)))

     ;; ISSUE: Dangerous. Directly calling private ABCL system function
     ;; to assign local instruments to symbols.
     ;;
     (system::%defparameter ,kick-name kd nil)
     (system::%defparameter ,snare-name sd nil)
     (system::%defparameter ,tom-name td nil)
     (system::%defparameter ,hat-name hhat nil)
     (system::%defparameter ,cymbal-name cym nil)
     (system::%defparameter ,tuned-name tuned nil)
     trap-kit))


(defun ampitheater (&key (parent pro1)
			 (kick-name 'pkick)
			 (snare-name 'psnare)
			 (tom-name 'ptom)
			 (hat-name 'phat)
			 (cymbal-name 'pcym)
			 (tuned-name 'ptuned))
  (standard-procussion-kit 'ampitheater
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cynbal-name
			   :tuned-name tuned-name))

(defun mega-drums (&key 
		   (parent pro1)
		   (kick-name 'pkick)
		   (snare-name 'psnare)
		   (tom-name 'ptom)
		   (hat-name 'phat)
		   (cymbal-name 'pcym)
		   (tuned-name 'ptuned))
  (standard-procussion-kit 'mega-drums 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun rock-n-roll (&key 
		    (parent pro1)
		    (kick-name 'pkick)
		    (snare-name 'psnare)
		    (tom-name 'ptom)
		    (hat-name 'phat)
		    (cymbal-name 'pcym)
		    (tuned-name 'ptuned))
  (standard-procussion-kit 'rock-n-roll 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun palladium (&key 
		  (parent pro1)
		  (kick-name 'pkick)
		  (snare-name 'psnare)
		  (tom-name 'ptom)
		  (hat-name 'phat)
		  (cymbal-name 'pcym)
		  (tuned-name 'ptuned))
  (standard-procussion-kit 'palladium 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun jazz-drums (&key 
		   (parent pro1)
		   (kick-name 'pkick)
		   (snare-name 'psnare)
		   (tom-name 'ptom)
		   (hat-name 'phat)
		   (cymbal-name 'pcym)
		   (tuned-name 'ptuned))
  (standard-procussion-kit 'jazz-drums 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun metal-drums (&key 
		    (parent pro1)
		    (kick-name 'pkick)
		    (snare-name 'psnare)
		    (tom-name 'ptom)
		    (hat-name 'phat)
		    (cymbal-name 'pcym)
		    (tuned-name 'ptuned))
  (standard-procussion-kit 'metal-drums 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun rap-session (&key 
		    (parent pro1)
		    (kick-name 'pkick)
		    (snare-name 'psnare)
		    (tom-name 'ptom)
		    (hat-name 'phat)
		    (cymbal-name 'pcym)
		    (tuned-name 'ptuned))
  (standard-procussion-kit 'rap-session 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun ambient-rock (&key 
		     (parent pro1)
		     (kick-name 'pkick)
		     (snare-name 'psnare)
		     (tom-name 'ptom)
		     (hat-name 'phat)
		     (cymbal-name 'pcym)
		     (tuned-name 'ptuned))
  (standard-procussion-kit 'ambient-rock 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun acoustic-kit (&key 
		     (parent pro1)
		     (kick-name 'pkick)
		     (snare-name 'psnare)
		     (tom-name 'ptom)
		     (hat-name 'phat)
		     (cymbal-name 'pcym)
		     (tuned-name 'ptuned))
  (standard-procussion-kit 'acoustic-kit 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun rock-drums (&key 
		   (parent pro1)
		   (kick-name 'pkick)
		   (snare-name 'psnare)
		   (tom-name 'ptom)
		   (hat-name 'phat)
		   (cymbal-name 'pcym)
		   (tuned-name 'ptuned))
  (standard-procussion-kit 'rock-drums 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun house-machine (&key 
		      (parent pro1)
		      (kick-name 'pkick)
		      (snare-name 'psnare)
		      (tom-name 'ptom)
		      (hat-name 'phat)
		      (cymbal-name 'pcym)
		      (tuned-name 'ptuned))
  (standard-procussion-kit 'house-machine 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun fusion-stix (&key 
		    (parent pro1)
		    (kick-name 'pkick)
		    (snare-name 'psnare)
		    (tom-name 'ptom)
		    (hat-name 'phat)
		    (cymbal-name 'pcym)
		    (tuned-name 'ptuned))
  (standard-procussion-kit 'fusion-stix 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun space-drums (&key 
		    (parent pro1)
		    (kick-name 'pkick)
		    (snare-name 'psnare)
		    (tom-name 'ptom)
		    (hat-name 'phat)
		    (cymbal-name 'pcym)
		    (tuned-name 'ptuned))
  (standard-procussion-kit 'space-drums 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun hard-rock (&key 
		  (parent pro1)
		  (kick-name 'pkick)
		  (snare-name 'psnare)
		  (tom-name 'ptom)
		  (hat-name 'phat)
		  (cymbal-name 'pcym)
		  (tuned-name 'ptuned))
  (standard-procussion-kit 'hard-rock 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun stadium-rox (&key 
		    (parent pro1)
		    (kick-name 'pkick)
		    (snare-name 'psnare)
		    (tom-name 'ptom)
		    (hat-name 'phat)
		    (cymbal-name 'pcym)
		    (tuned-name 'ptuned))
  (standard-procussion-kit 'stadium-rox 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun dance-2000 (&key 
		   (parent pro1)
		   (kick-name 'pkick)
		   (snare-name 'psnare)
		   (tom-name 'ptom)
		   (hat-name 'phat)
		   (cymbal-name 'pcym)
		   (tuned-name 'ptuned))
  (standard-procussion-kit 'dance-2000 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun heavy-metal (&key 
		    (parent pro1)
		    (kick-name 'pkick)
		    (snare-name 'psnare)
		    (tom-name 'ptom)
		    (hat-name 'phat)
		    (cymbal-name 'pcym)
		    (tuned-name 'ptuned))
  (standard-procussion-kit 'heavy-metal 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun hip-hop (&key 
		(parent pro1)
		(kick-name 'pkick)
		(snare-name 'psnare)
		(tom-name 'ptom)
		(hat-name 'phat)
		(cymbal-name 'pcym)
		(tuned-name 'ptuned))
  (standard-procussion-kit 'hip-hop 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun sluggo-drums (&key 
		     (parent pro1)
		     (kick-name 'pkick)
		     (snare-name 'psnare)
		     (tom-name 'ptom)
		     (hat-name 'phat)
		     (cymbal-name 'pcym)
		     (tuned-name 'ptuned))
  (standard-procussion-kit 'sluggo-drums 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun huge-room (&key 
		  (parent pro1)
		  (kick-name 'pkick)
		  (snare-name 'psnare)
		  (tom-name 'ptom)
		  (hat-name 'phat)
		  (cymbal-name 'pcym)
		  (tuned-name 'ptuned))
  (standard-procussion-kit 'huge-room 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun drum-dance (&key 
		   (parent pro1)
		   (kick-name 'pkick)
		   (snare-name 'psnare)
		   (tom-name 'ptom)
		   (hat-name 'phat)
		   (cymbal-name 'pcym)
		   (tuned-name 'ptuned))
  (standard-procussion-kit 'drum-dance 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun heavyosity (&key 
		   (parent pro1)
		   (kick-name 'pkick)
		   (snare-name 'psnare)
		   (tom-name 'ptom)
		   (hat-name 'phat)
		   (cymbal-name 'pcym)
		   (tuned-name 'ptuned))
  (standard-procussion-kit 'heavyosity 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun dance-club (&key 
		   (parent pro1)
		   (kick-name 'pkick)
		   (snare-name 'psnare)
		   (tom-name 'ptom)
		   (hat-name 'phat)
		   (cymbal-name 'pcym)
		   (tuned-name 'ptuned))
  (standard-procussion-kit 'dance-club 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun country-kit (&key 
		    (parent pro1)
		    (kick-name 'pkick)
		    (snare-name 'psnare)
		    (tom-name 'ptom)
		    (hat-name 'phat)
		    (cymbal-name 'pcym)
		    (tuned-name 'ptuned))
  (standard-procussion-kit 'country-kit 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))

(defun rockabilly (&key 
		   (parent pro1)
		   (kick-name 'pkick)
		   (snare-name 'psnare)
		   (tom-name 'ptom)
		   (hat-name 'phat)
		   (cymbal-name 'pcym)
		   (tuned-name 'ptuned))
  (standard-procussion-kit 'rockabilly 
			   :parent parent
			   :kick-name kick-name
			   :snare-name snare-name
			   :tom-name tom-name
			   :hat-name hat-name
			   :cymbal-name cymbal-name
			   :tuned-name tuned-name))
