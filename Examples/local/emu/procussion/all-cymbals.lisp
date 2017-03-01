;;;; emu procussion all-cymbals
;;;;
;;;;  all-cymbals
;;;;     hata
;;;;     hatb
;;;;     sixteen  - 16" cymbal
;;;;     pang
;;;;     ride
;;;;     allcym-dark
;;;;     allcym-gong
;;;;     allcym-efx
;;;;
;;;; Zone Stack
;;;;  1 507 Gong Pow      : 036 037                                                               
;;;;  2 233 HihatA 1/3    : 038 039                                                            
;;;;  3 234 HihatA 2/3    : 040 041                                                           
;;;;  4 235 HihatA Open   : 042 043                                                             
;;;;  5 236 HihatA stomp  : 044 045                            
;;;;  6 286 16"Cymbal 1   : 046 047                                                            
;;;;  7 287 16"Choke 1    : 048 049                                                         
;;;;  8 288 16"Crash 2    : 050 051                                                         
;;;;  9 289 19"Pang       : 052 053                                                        
;;;; 10 290 Ride Ping     : 054 056                                                              
;;;; 11 291 Ride Bell     : 057 059                                                            
;;;; 12 298 HyeRideCym    : 060 063                                                            
;;;; 13 302 DarkCymbl1    : 064 066                                                             
;;;; 14 303 DarkCymb2     : 067 069                               
;;;; 15 309 GongODoom     : 070 071                                                         
;;;; 16 318 Hyperreal     : 072 074                                  
;;;; 17 317 StereoMalt    : 075 076                                               
;;;; 18 321 NoiseBurst    : 077 078                                               
;;;; 19 493 IceMissile    : 079 083                                              
;;;; 20 237 HihatB Shut   : 084 085                                      
;;;; 21 238 HiHatB 1/3    : 086 087                                      
;;;; 22 239 HihatB 2/3    : 088 089                                      
;;;; 23 240 HihatB Open   : 090 091                                      
;;;; 24 241 HihatB stomp  : 092 093 
;;;;

(in-package :cyco)

(let*((program-number (car (cdr (assoc 'all-cymbals +PROCUSSION-PROGRAMS+))))
      (hata-map (keymap 'hatA '((X      44 "HihatA stomp")
				(op     38 "HihatA 1/3")
				(opn    40 "HihatA 2/3")
				(open   42 "HihatA Open")
				(stomp  44 "HihatA stomp")
				(X2     45 "HihatA stomp")
				(op2    39 "HihatA 1/3")
				(opn2   41 "HihatA 2/3")
				(open2  43 "HihatA Open")
				(stomp2 45 "HihatA stomp"))))
      (hatb-map (keymap 'hatA '((X      84 "HihatB shut")
				(op     86 "HihatB 1/3")
				(opn    88 "HihatB 2/3")
				(open   90 "HihatB Open")
				(stomp  92 "HihatB stomp")
				(X2     85 "HihatB shut")
				(op2    87 "HihatB 1/3")
				(opn2   89 "HihatB 2/3")
				(open2  91 "HihatB Open")
				(stomp2 93 "HihatB stomp"))))
      (sixteen-map (keymap 'sixteen-inch '((X       46 "16' Cymbal 1")
					   (choke   48 "16' Choke 1")
					   (crash   50 "16' Crash 2")
					   (X2      46 "16' Cymbal 1")
					   (choke2  48 "16' Choke 1")
					   (crash2  50 "16' Crash 2"))))
      (pang-map (keymap 'pang '((x  52 "19' Pang")
				(x2 53 "19' Pang"))))
      (ride-map (keymap 'ride '((ping   54 "Ride Ping")
				(belll  57 "Ride Bell")
				(A      60 "HyeRideCym")
				(B      72 "Hyperreal")
				(ping2  55 "Ride Ping")
				(belll2 58 "Ride Bell")
				(A2     61 "HyeRideCym")
				(B2     73 "Hyperreal")
				(ping3  56 "Ride Ping")
				(belll3 59 "Ride Bell")
				(A3     62 "HyeRideCym")
				(B3     74 "Hyperreal"))))
      (dark-map (keymap 'dark '((A     64  "DarkCymbl1")
				(B     67  "DarkCymbal2")
				(roll  72  "StereoMalt")
				(A2    65  "DarkCymbl1")
				(B2    68  "DarkCymbal2")
				(roll2 73  "StereoMalt")
				(A3    66  "DarkCymbl1")
				(B3    69  "DarkCymbal2"))))
      (gong-map (keymap 'gong '((A  36 "Gong Pow")
				(B  70 "GongODoom")
				(A2 37 "Gong Pow")
				(B2 71 "GongOdoom"))))
      (efx-map (keymap 'efx '((noise  77 "Noise Burst")
			      (noise2 78 "Noise Burst")
			      (ice    79 "IceMissile")
			      (ice2   80 "IceMissile")
			      (ice3   81 "IceMissile")))))
 
  (param all-cymbals nil)
  (defun all-cymbals (&key (parent pro2))
    (setf all-cymbals (create-instrument 'all-cymbals
					 :parent parent
					 :transient t
					 :program-change-hook
					 (constant-program-hook
					  'all-cymbals program-number)))
    (param hata (create-instrument 'hata
				   :parent all-cymbals
				   :keynumber-map hata-map))
    (param hatb (create-instrument 'hatb
				   :parent all-cymbals
				   :keynumber-map hatb-map))
    (param sixteen (create-instrument 'sixteen
				      :parent all-cymbals
				      :keynumber-map sixteen-map))
    (param pang (create-instrument 'pang
				   :parent all-cymbals
				   :keynumber-map pang-map))
    (param ride (create-instrument 'ride
				   :parent all-cymbals
				   :keynumber-map ride-map))
    (param allcym-dark (create-instrument 'allcym-dark
				   :parent all-cymbals
				   :keynumber-map dark-map))
    (param allcym-gong (create-instrument 'allcym-gong
				   :parent all-cymbals
				   :keynumber-map gong-map))
    (param allcym-efx (create-instrument 'allcym-efx
					 :parent all-cymbals
					 :keynumber-map efx-map))
    all-cymbals))
  
