;;;; cyco local quantumleap mor2-drums
;;;;

(in-package :cyco)


;;; fl -> indicates "full load" instrument
;;; ll -> indicates "light load" instrument
;;;
(defkeymap --m2-kick-map-fl 
  '((A          .  35)
    (B          .  36)
    (C          .  59)
    (D          .  60)))

(defkeymap --m2-kick-map-ll 
  '((A          .  35)
    (B          .  36)
    (C          .  35)
    (D          .  36)))

(defkeymap --m2-snare-map-fl
  '((X          .  38)
    (RIM        .  37)
    (CRACK      .  39)
    (EDGE       .  40)
    (BOUNCE     .  62)
    (FLAM       .  63)
    (ROLL       .  64)
    (X2         .  86)
    (RIM2       .  85)
    (CRACK2     .  87)
    (EDGE2      .  88)
    (A          .  (38 "Emu Compatability -> X"))
    (B          .  (40 "Emu Compatibility -> Edge"))
    (C          .  (62 "Emu Compatibility -> Bounce"))
    (D          .  (37 "Emu Compatibility -> Crack"))))


(defkeymap --m2-snare-map-ll 
  '((X          .  38)
    (RIM        .  37)
    (CRACK      .  39)
    (EDGE       .  40)
    (BOUNCE     .  40)
    (FLAM       .  38)
    (ROLL       .  40)
    (X2         .  38)
    (RIM2       .  37)
    (CRACK2     .  39)
    (EDGE2      .  40)
    (A          .  (38 "Emu Compatability -> X"))
    (B          .  (40 "Emu Compatibility -> Edge"))
    (C          .  (62 "Emu Compatibility -> Bounce"))
    (D          .  (37 "Emu Compatibility -> Crack"))))

(defkeymap --m2-tom-map-fl 
  '((A          .  41)
    (B          .  43)
    (C          .  45)
    (D          .  47)
    (E          .  48)
    (F          .  50)
    (A-FLAM     .  65)
    (B-FLAM     .  67)
    (C-FLAM     .  69)
    (D-FLAM     .  71)
    (E-FLAM     .  72)
    (F-FLAM     .  74)
    (A-BOUNCE   .  89)
    (B-BOUNCE   .  91)
    (C-BOUNCE   .  93)
    (D-BOUNCE   .  95)
    (E-BOUNCE   .  96)
    (F-BOUNCE   .  98)))
    ;; (A          .  (38 "Emu Compatability -> X"))
    ;; (B          .  (40 "Emu Compatibility -> Edge"))
    ;; (C          .  (62 "Emu Compatibility -> Bounce"))
    ;; (D          .  (37 "Emu Compatibility -> Crack"))))

(defkeymap --m2-tom-map-ll 
  '((A          .  41)
    (B          .  43)
    (C          .  45)
    (D          .  47)
    (E          .  48)
    (F          .  50)
    (A-FLAM     .  41)
    (B-FLAM     .  43)
    (C-FLAM     .  45)
    (D-FLAM     .  47)
    (E-FLAM     .  48)
    (F-FLAM     .  50)
    (A-BOUNCE   .  41)
    (B-BOUNCE   .  43)
    (C-BOUNCE   .  45)
    (D-BOUNCE   .  47)
    (E-BOUNCE   .  48)
    (F-BOUNCE   .  50)))

(defkeymap --m2-cow-map 
  '((A          .  32)
    (B          .  31)
    (C          .  33)))

(defkeymap --m2-hat-map-fl 
  '((X          .  42)
    (OP         .  44)
    (OPN        .  46)
    (OPEN       .  49)
    (PED        .  34)
    (EDGE       .  66)
    (EOP        .  68)
    (EOPN       .  70)
    (EOPEN      .  73)
    (BELL       .  90)
    (BOP        .  92)
    (BOPN       .  94)
    (BOPEN      .  97)))

(defkeymap --m2-hat-map-ll 
  '((X          .  42)
    (OP         .  44)
    (OPN        .  46)
    (OPEN       .  49)
    (PED        .  34)
    (EDGE       .  42)
    (EOP        .  44)
    (EOPN       .  46)
    (EOPEN      .  49)
    (BELL       .  42)
    (BOP        .  44)
    (BOPN       .  46)
    (BOPEN      .  49)))

(defkeymap --m2-cym-map-fl 
  '((RIDE       .  51)
    (A          .  52)
    (A-CHOKE    .  53)
    (B          .  54)
    (B-CHOKE    .  55)
    (C          .  56)
    (C-CHOKE    .  57)
    (EDGE       .  75)
    (A-MID      .  76)
    (B-MID      .  78)
    (BELL       .  99)
    (BELL2      . 100)))

(defkeymap --m2-cym-map-ll 
  '((RIDE       .  51)
    (A          .  52)
    (A-CHOKE    .  53)
    (B          .  54)
    (B-CHOKE    .  55)
    (C          .  56)
    (C-CHOKE    .  57)
    (EDGE       .  51)
    (A-MID      .  52)
    (B-MID      .  54)
    (BELL       .  99)
    (BELL2      . 100)))

(defkeymap --m2-ride-map-fl 
  '((X          .  51)
    (EDGE       .  75)
    (BELL       .  99)
    (BELL2      . 100)))

(defkeymap --m2-ride-map-ll 
  '((X          .  51)
    (EDGE       .  51)
    (BELL       .  51)
    (BELL2      .  51)))

(param m2-drums nil)
(param m2-kick nil)
(param m2-snare nil)
(param m2-tom nil)
(param m2-cow nil)
(param m2-hat nil)
(param m2-cym nil)
(param m2-ride nil)

(setf m2-drums (create-instrument
		'm2-drums
		:parent QL5
		:transient nil))

(defun --m2-drums-light (parent)
  (free-orchestra! :node parent)
  (param m2-kick (create-instrument
		  'm2-kick
		  :parent parent
		  :transient t
		  :keynumber-map #'--m2-kick-map-ll))
  (param m2-snare (create-instrument
		   'm2-snare
		   :parent parent
		   :transient t
		   :keynumber-map #'--m2-snare-map-ll))
  (param m2-tom (create-instrument
		 'm2-tom
		 :parent parent
		 :transient t
		 :keynumber-map #'--m2-tom-map-ll))
  (param m2-cow (create-instrument
		 'm2-cow
		 :parent parent
		 :transient t
		 :keynumber-map #'--m2-cow-map))
  (param m2-hat (create-instrument
		 'm2-hat
		 :parent parent
		 :transient t
		 :keynumber-map #'--m2-hat-map-ll))
  (param m2-cym (create-instrument
		 'm2-cym
		 :parent parent
		 :transient t
		 :keynumber-map #'--m2-cym-map-ll))
  (param m2-ride (create-instrument
		  'm2-ride
		  :parent parent
		  :transient t
		  :keynumber-map #'--m2-ride-map-ll))
  m2-drums)

(defun --m2-drums-full (parent)
  (free-orchestra! :node parent)
  (param m2-kick (create-instrument
		  'm2-kick
		  :parent parent
		  :transient t
		  :keynumber-map #'--m2-kick-map-fl))
  (param m2-snare (create-instrument
		   'm2-snare
		   :parent parent
		   :transient t
		   :keynumber-map #'--m2-snare-map-fl))
  (param m2-tom (create-instrument
		 'm2-tom
		 :parent parent
		 :transient t
		 :keynumber-map #'--m2-tom-map-fl))
  (param m2-cow (create-instrument
		 'm2-cow
		 :parent parent
		 :transient t
		 :keynumber-map #'--m2-cow-map))
  (param m2-hat (create-instrument
		 'm2-hat
		 :parent parent
		 :transient t
		 :keynumber-map #'--m2-hat-map-fl))
  (param m2-cym (create-instrument
		 'm2-cym
		 :parent parent
		 :transient t
		 :keynumber-map #'--m2-cym-map-fl))
  (param m2-ride (create-instrument
		  'm2-ride
		  :parent parent
		  :transient t
		  :keynumber-map #'--m2-ride-map-fl))
  m2-drums)
  

(defun m2-drums (&key (light-load nil)(parent m2-drums))
  (if light-load
      (--m2-drums-light parent)
    (--m2-drums-full parent)))
		      
