;;;; emu procussion cymbals2
;;;;
;;;;  cymbals2
;;;;     hats
;;;;     ride
;;;;     sixteen
;;;;     crash
;;;;     pingpang
;;;;     gong
;;;;     mallet-cymbal
;;;;
;;;;
;;;; Zone Stack
;;;;  1 241 HiHatBStmp    : 024 025                                                               
;;;;  2 237 HiHatBshut    : 026 027                                                            
;;;;  3 238 HiHatB 1/3    : 028 029                                                           
;;;;  4 239 HiHatB 2/3    : 030 031                                                             
;;;;  5 240 HiHatBopen    : 032 033
;;;;  6 301 Vlcty Ride    : 036 037                                                            
;;;;  7 298 HypeRideCym   : 038 039                                                         
;;;;  8 318 Hypereal      : 040 041
;;;;  9 290 Ride Ping     : 042 043 044                                                    
;;;; 10 291 Ride Bell     : 045 046 047
;;;; 11 286 16"Crash      : 048                                                                
;;;; 12 287 16"Choke      : 049
;;;; 13 298 Dbl Crash     : 050 051                                                             
;;;; 14 302 DarkCymbal1   : 052 053                               
;;;; 15 303 DarkCymbal2   : 054 055
;;;; 16 293 China Gong    : 059 060                                  
;;;; 17 299 PangCymbal    : 061 062                                               
;;;; 18 314 SmashCymbl    : 063 064                                               
;;;; 19 306 SplashDance   : 065 068                                              
;;;; 20 308 FingerCymb    : 069 071
;;;; 21 309 Gong Doom     : 072 080                                      
;;;; 22 294 MalletRoll    : 080 083                                      
;;;; 23 292 MalCymbal     : 084 092                                      
;;;; 24 317 StereoMalt    : 093 127                       
;;;;

(in-package :cyco)
(param cymbals2 nil)
(let*((program-number (car (cdr (assoc 'cymbals2 +PROCUSSION-PROGRAMS+))))
      (hat-map (keymap 'c3-hats '((x      26)
				  (stomp  24)
				  (op     28)
				  (opn    30)
				  (open   32)
				  (x2     27)
				  (stomp2 25)
				  (op2    29)
				  (opn2   31)
				  (open2  33))))
      (ride-map (keymap 'c3-ride '((A  36 "Vlcty Ride")
				   (B  38 "HypeRideCym")
				   (C  40 "Hypereal")
				   (A2 37 "Vlcty Ride")
				   (B2 39 "HypeRideCym")
				   (C2 41 "Hypereal")
				   (ping  42)
				   (bell  45)
				   (ping2 43)
				   (bell2 46)
				   (ping3 44)
				   (bell3 47))))
      (sixteen-map (keymap 'c3-16inch '((crash 48)(choke 49))))
      (crash-map  (keymap 'c3-crash '((A  50 "DblCrash")
				      (B  52 "DarkCymbal1")
				      (C  54 "DarkCymbal2")
				      (A2 51 "DblCrash")
				      (B2 53 "DarkCymbal1")
				      (C2 55 "DarkCymbal2"))))
      (pingpang-map (keymap 'pingpang '((ping    42 "ride-ping")
					(bell    46 "ride-bell")
					(china   59 "china Gong")
					(pang    61 "PangCymbal")
					(smash   63 "SmashCymbl")
					(splash  65 "SplashDance")
					(finger  69 "FingerCymb")
					(ping2   42 "ride-ping")
					(bell2   46 "ride-bell")
					(china2  59 "china Gong")
					(pang2   61 "PangCymbal")
					(smash2  63 "SmashCymbl")
					(splash2 65 "SplashDance")
					(finger2 69 "FingerCymb"))))
      (gong-map (circular-keymap 'gong (range 72 80)))
      (mallet-map (keymap 'mallet-cymbal '((roll  80  "MalletRoll")
					   (mal   84  "MalCymbal")
					   (malt  93  "StereoMalt")
					   (roll2  81  "MalletRoll")
					   (mal2   85  "MalCymbal")
					   (malt2  94  "StereoMalt")
					   (roll3  82  "MalletRoll")
					   (mal3   86  "MalCymbal")
					   (malt3  95  "StereoMalt")))))
  (defun cymbals2 (&key (parent pro2))
    (setf cymbals2 (create-instrument 'cymbals2
					 :parent parent
					 :transient t
					 :program-change-hook
					 (constant-program-hook
					  'cymbals2 program-number)))
    (param hats (create-instrument 'hats
				   :parent cymbals2
				   :keynumber-map hat-map))
    (param ride (create-instrument 'ride
				   :parent cymbals2
				   :keynumber-map ride-map))
    (param sixteen (create-instrument 'sixteen
				      :parent cymbals2
				      :keynumber-map sixteen-map))
    (param crash (create-instrument 'crash
				    :parent cymbals2
				    :keynumber-map crash-map))
    (param pingpang (create-instrument 'pingpang
				       :parent cymbals2
				       :keynumber-map pingpang-map))
    (param gong (create-instrument 'gong
				   :parent cymbals2
				   :keynumber-map gong-map))
    (param mallet-cymbal (create-instrument 'mallet-cymbal
					    :parent cymbals2
					    :keynumber-map mallet-map))
    cymbals2))
  
