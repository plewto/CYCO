;;;; cyco emu procussion cymbals
;;;;
;;;; Instruments:
;;;;     cymbals
;;;;        hat
;;;;        ride
;;;;        crash
;;;;        pingpang
;;;;        gong
;;;;        cymroll

(in-package :cyco)

(defkeymap --procussion-cymbals-hat-map '((x     .  026)
					  (OP    .  028)
					  (OPN   .  030)
					  (OPEN  .  032)
					  (ped   .  024)
					  (x2    .  027)
					  (OP2   .  029)
					  (OPN2  .  031)
					  (OPEN2 .  033)
					  (PED2  .  025)
					  (EDGE  . (027 "MOR2 Compatibility"))
					  (EOP   . (029 "MOR2 Compatibility"))
					  (EOPN  . (031 "MOR2 Compatibility"))
					  (EOPEN . (033 "MOR2 Compatibility"))
					  (BELL  . (027 "MOR2 Compatibility"))
					  (BOP   . (029 "MOR2 Compatibility"))
					  (BOPN  . (031 "MOR2 Compatibility"))
					  (BOPEN . (033 "MOR2 Compatibility"))))

(defkeymap --procussion-cymbals-ride-map '((A         .  036)
					   (B         .  039)
					   (C         .  041)
					   (PING      .  043)
					   (BELL      .  046)
					   (A2        .  037)
					   (AB        .  038)
					   (BC        .  040)
					   (PING2     .  044)
					   (PING-BELL .  045)
					   (BELL2     .  047)))

(defkeymap --procussion-cymbals-crash-map '((A        . 048)
					    (CHOKE    . 049)
					    (DBL      . 050)
					    (DARK     . 052)
					    (XDARK1   . 053)
					    (XDARK2   . 054)
					    (XDARK3   . 055)
					    (XDARK4   . 056)
					    (XDARK5   . 057)
					    (DARK2    . 058)
					    (DARK3    . 059)))

(defkeymap --procussion-cymbals-pingpang-map '((china       . 060)
					       (pang        . 062)
					       (smash       . 064)
					       (splash      . 066)
					       (finger      . 070)
					       (chinapang   . 061)
					       (smashpang   . 063)
					       (smashsplash . 065)
					       (splash2     . 067)
					       (fsplash     . 068)
					       (fsplash2    . 069)
					       (finger2     . 071)))

(defkeymap --procussion-cymbals-gong-map  '((gong     .  072)
					    (gong2    .  073)
					    (gong3    .  074)
					    (xgong    .  075)
					    (xgong2   .  076)
					    (xgong3   .  077)
					    (xmallet3 .  078)
					    (xmallet2 .  079)
					    (xmallet  .  080)
					    (mallet3  .  081)		    
					    (mallet2  .  082)
					    (mallet   .  083)))

(defkeymap --procussion-cymbals-cymroll-map '((mall   .  084)
					      (mall2  .  085)
					      (mall3  .  086)
					      (xmall  .  087)
					      (xmall2 .  088)
					      (xmall3 .  089)
					      (xsmal3 .  090)
					      (xsmal2 .  091)
					      (xsmal1 .  092)
					      (smal2  .  093)
					      (smal1  .  094)
					      (smal   .  095))) 

(param cymbals nil)
(param hat nil)
(param ride nil)
(param crash nil)
(param pingpang nil)
(param gong nil)
(param cymroll nil)

(defun cymbals (&key (parent pro2))
  (let ((program-number (car (cdr (assoc 'cymbal-kit-1 +PROCUSSION-PROGRAMS+)))))
    (setf cymbals (create-instrument
		   'cymbals
		   :parent parent
		   :transient t
		   :program-change-hook (constant-program-hook
					 'cymbals program-number)))
    (setf hat (create-instrument
	       'hat
	       :parent cymbals
	       :keynumber-map #'--procussion-cymbals-hat-map))

    (setf ride (create-instrument
		'ride
		:parent cymbals
		:keynumber-map #'--procussion-cymbals-ride-map))

    (setf crash (create-instrument
		 'crash
		 :parent cymbals
		 :keynumber-map #'--procussion-cymbals-crash-map))

    (setf pingpang (create-instrument
		    'pingpang
		    :parent cymbals
		    :keynumber-map #'--procussion-cymbals-pingpang-map))

    (setf gong (create-instrument
		'gong
		:parent cymbals
		:keynumber-map #'--procussion-cymbals-gong-map))

    (setf cymroll (create-instrument
		   'cymroll
		   :parent cymbals
		   :keynumber-map #'--procussion-cymbals-cymroll-map))

    cymbals))



