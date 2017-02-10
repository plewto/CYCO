;;;; cyco/Local/Emu/Procussion/idophones1.lisp
;;;; 2013.04.18
;;;; Instruments:
;;;;    ido1
;;;;        ido1.cabasa
;;;;        ido1.net (shaker net)
;;;;        ido1.crash
;;;;        ido1.ride
;;;;        ido1.cowclave
;;;;        ido1.hat
;;;;        ido1.shakers (combined cabasa and net)
;;;;        ido1.cymbals (combined crash and ride)
;;;;
;;;;
;;;;
;;;; Zn Rang       Stack              Tune Fine   Group1      Group2  
;;;; 01 036 - 037  389 Cabasa Front -  ..   ..  - Cabasa    - Shaker
;;;; 02 038 - 039  390 Cabasa Back  -  ..   ..  - Cabasa    - Shaker
;;;; 03 040 - 041  391 Cabasa Roll  -  ..   ..  - Cabasa    - Shaker
;;;; 04 042 - 043  379 Shakatam     - -03   ..  - Tam       - Shaker
;;;; 05 044 - 045  368 Tambourine   - -01   ..  - Tam       - Shaker
;;;; 06 046 - 047  286 16" Crash 1  -  ..   ..  - Crash     - Cym
;;;; 07 048 - 049  287 16" Choke 1  -  ..   ..  - Crash     - Cym
;;;; 08 050 - 051  288 16" Crash 2  -  ..   ..  - Crash     - Cym
;;;; 09 052 - 053  394 ShakerNet    -  ..   ..  - Net       - Shaker
;;;; 10 054 - 056  395 ShakerSnp    -  ..   ..  - Net       - Shaker
;;;; 11 057 - 059  291 Ride Bell    -  ..   ..  - Ride      - Cym
;;;; 12 060 - 063  298 HypRidCym    -  ..   ..  - Ride      - Cym 
;;;; 13 064 - 066  302 DarkCymbl1   -  ..   ..  - Crash     - Cym
;;;; 14 067 - 069  303 DarkCymbl2   -  ..   ..  - Crash     - Cym
;;;; 15 070 - 071  396 ShakerBack   -  ..   ..  - Net       - Shaker
;;;; 16 072 - 074  318 Hyperreal    - +03   ..  - Ride      - Cym
;;;; 17 075 - 076  385 CampanaOp    -  ..  +17  - Cow-clave
;;;; 18 077 - 078  386 CampanaHel   -  ..  +12  - Cow-clave
;;;; 19 094 - 127  365 Clave        -  ..   ..  - Cow-clave
;;;; 20 084 - 085  237 HiHatBshut   -  ..   ..  - Hat
;;;; 21 086 - 087  238 HiHatB 1/3   -  ..   ..  - Hat
;;;; 22 088 - 089  239 HiHatB 2/3   -  ..   ..  - Hat
;;;; 23 090 - 091  240 HiHatB Op    -  ..   ..  - Hat
;;;; 24 092 - 093  241 HiHatB Stomp -  ..   ..  - Hat


(in-package :cm)


;;;; 01 036 - 037  389 Cabasa Front -  ..   ..  - Cabasa
;;;; 02 038 - 039  390 Cabasa Back  -  ..   ..  - Cabasa
;;;; 03 040 - 041  391 Cabasa Roll  -  ..   ..  - Cabasa
;;;;
(defmap --idophones1-cabasa-map '((FRNT  . 036)
				  (BACK  . 038)
				  (ROLL  . 040)
				  (FRNT2 . 037)
				  (BACK2 . 039)
				  (ROLL2 . 041)
				  (X     . 036)))

;;;; 09 052 - 053  394 ShakerNet    -  ..   ..  - Net
;;;; 10 054 - 056  395 ShakerSnp    -  ..   ..  - Net
;;;; 15 070 - 071  396 ShakerBack   -  ..   ..  - Net
;;;;
(defmap --idophones1-net-map '((NET    . 052)
			       (SNAP   . 054)
			       (BACK   . 070)
			       (NET2   . 053)
			       (SNAP2  . 055)
			       (BACK2  . 071)
			       (SNAP3  . 056)
			       (X      . 052)))

;;;; 04 042 - 043  379 Shakatam     - -03   ..  - Tam
;;;; 05 044 - 045  368 Tambourine   - -01   ..  - Tam
;;;;
(defmap --idophones1-tambourine-map '((A . 042)
				      (B . 044)
				      (C . 043)
				      (D . 045)
				      (X . 042)))

;;;; 06 046 - 047  286 16" Crash 1  -  ..   ..  - Crash
;;;; 07 048 - 049  287 16" Choke 1  -  ..   ..  - Crash
;;;; 08 050 - 051  288 16" Crash 2  -  ..   ..  - Crash
;;;; 13 064 - 066  302 DarkCymbl1   -  ..   ..  - Crash
;;;; 14 067 - 069  303 DarkCymbl2   -  ..   ..  - Crash
;;;;
(defmap --idophones1-crash-map '((CRASH   . 046)
				 (CRASH2  . 050)
				 (CRASH3  . 047)
				 (CRASH4  . 051)
				 (DARK    . 064)
				 (DARK2   . 067)
				 (DARK3   . 065)
				 (DARK4   . 068)
				 (DARK5   . 066)
				 (DARK6   . 069)
				 (CHOKE   . 048)
				 (CHOKE2  . 049)
				 (CHOKE   . 049)
				 (X       . 046)))
						   

;;;; 11 057 - 059  291 Ride Bell    -  ..   ..  - Ride
;;;; 12 060 - 063  298 HypRidCym    -  ..   ..  - Ride
;;;; 16 072 - 074  318 Hyperreal    - +03   ..  - Ride
;;;;
(defmap --idophones1-ride-map '((RIDE    .  060)
				(HYPER   .  072)
				(BELL    .  057)
				(RIDE2   .  061)
				(HYPER2  .  073)
				(BELL2   .  058)
				(RIDE3   .  062)
				(HYPER3  .  074)
				(BELL3   .  059)
				(RIDE4   .  063)
				(X       .  060)))

;;;; 17 075 - 076  385 CampanaOp    -  ..  +17  - Cow-clave
;;;; 18 077 - 078  386 CampanaHel   -  ..  +12  - Cow-clave
;;;; 19 094 - 127  365 Clave        -  ..   ..  - Cow-clave
;;;;
(defmap --idophones1-cowclave-map '((OPEN    . 075)
				    (HEEL    . 077)
				    (OPEN2   . 076)
				    (HELL2   . 078)
				    (CLAVE   . 103)
				    (CLAVE2  . 104)
				    (CLAVE3  . 105)
				    (CLAVE4  . 106)
				    (CLAVE5  . 107)
				    (CLAVE6  . 108)
				    (CLAVE7  . 109)
				    (CLAVE8  . 110)
				    (CLAVE9  . 111)
				    (CLAVE10 . 112)
				    (CLAVE11 . 113)
				    (CLAVE12 . 114)
				    (CLAVE13 . 115)
				    (X       . 075)))
					     
;;;; 20 084 - 085  237 HiHatBshut   -  ..   ..  - Hat
;;;; 21 086 - 087  238 HiHatB 1/3   -  ..   ..  - Hat
;;;; 22 088 - 089  239 HiHatB 2/3   -  ..   ..  - Hat
;;;; 23 090 - 091  240 HiHatB Op    -  ..   ..  - Hat
;;;; 24 092 - 093  241 HiHatB Stomp -  ..   ..  - Hat
;;;;
(defmap --idophones1-hat-map '((x      . 084)
			       (op     . 086)
			       (opn    . 088)
			       (open   . 090)
			       (stomp  . 092)
			       (x2     . 085)
			       (op2    . 087)
			       (opn2   . 089)
			       (open2  . 091)
			       (stomp2 . 093)))

;; Combines Cabasa, Tambourine and Shaker net
;;
(defmap --idophones1-shakers-map '((FRNT   . 036)
				   (BACK   . 038)
				   (ROLL   . 040)
				   (SHAKE  . 042)
				   (TAM    . 044)
				   (NET    . 052)
				   (SNAP   . 054)
				   (NBACK  . 070)
				   (FRNT2  . 037)
				   (BACK2  . 039)
				   (ROLL2  . 040)
				   (SHAKE2 . 043)
				   (TAM2   . 045)
				   (NET2   . 053)
				   (SNAP2  . 055)
				   (NBACK2 . 071)
				   (SNAP3  . 056)))	

;; Combines CRASH, DARK, CHOKE, RIDE, BELL AND HYPER-RIDE
;;
(defmap --idophones1-cymbals-map '((CRASH  . 046)
				   (CRASH2 . 050)
				   (DARK   . 064)
				   (DARK2  . 067)
				   (CHOKE  . 048)
				   (BELL   . 057)
				   (RIDE   . 060)
				   (HYPER  . 072)
				   (CRASH3 . 047)
				   (CRASH4 . 051)
				   (DARK3  . 065)
				   (DARK4  . 068)
				   (CHOKE2 . 049)
				   (BELL2  . 058)
				   (RIDE2  . 061)
				   (HYPER2 . 073)
				   (DARK5  . 067)
				   (DARK6  . 069)
				   (BELL3  . 059)
				   (RIDE3  . 062)
				   (HYPER3 . 074)
				   (RIDE4  . 064)))

(param ido1 nil)

(defun idophones1 (&optional (parent pro3)(program 'idophones1))
  (remove-children parent)
  (set-value parent :program (procussion-program-map program))
  (let ((kinst (instrument 'idophones1 :parent parent)))
    (param ido1.cabasa (instrument
		       :ido1.cabasa
		       :parent kinst
		       :keymap #'--idophones1-cabasa-map))
    (param ido1.net (instrument
		       :ido1.net
		       :parent kinst
		       :keymap #'--idophones1-net-map))
    (param ido1.tambourine (instrument
			   :ido1.tambourine
			   :parent kinst
			   :keymap #'--idophones1-tambourine-map))
    (param ido1.crash (instrument
		      :ido1.crash
		      :parent kinst
		      :keymap #'--idophones1-crash-map))
    (param ido1.ride (instrument
		     :ido1.ride
		     :parent kinst
		     :keymap #'--idophones1-ride-map))
    (param ido1.cowclave (instrument
			 :ido1.cowclave
			 :parent kinst
			 :keymap #'--idophones1-cowclave-map))
    (param ido1.hat (instrument
		    :ido1.hat
		    :parent kinst
		    :keymap #'--idophones1-hat-map))
    (param ido1.shakers (instrument
		       :ido1.shaker
		       :parent kinst
		       :keymap #'--idophones1-shakers-map))
    (param ido1.cymbals (instrument
			:ido1.cymbals
			:parent kinst
			:keymap #'--idophones1-cymbals-map))
    (setf ido1 kinst)
    ido1))
