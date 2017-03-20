;;;; emu procussion rocket-drums
;;;; 2013.04.22
;;;; Instruments          
;;;;    rocket-drums
;;;;        rd-kick
;;;;        rd-snare
;;;;        rd-cymbal
;;;;        rd-tomic
;;;;        rd-roto
;;;;
;;;; 01	:021 MansionKick   : 036 - xxx kick                                  
;;;; 02	:044 HouseKick2    : 038 - xxx kick                                 
;;;; 03	:105 ModernTwin    : 040 - xxx snr                                   
;;;; 04	:173 CentrRvers    : 045 - xxx snr                                   
;;;; 05	:099 ModernSnare   : 041 - xxx snr
;;;; 06	:106 SnareCenter   : 043 - xxx snr                                   
;;;; 07	:257 Elec Hat 1    : 037 - xxx cym                                   
;;;; 08	:258 Elec Hat 2    : 039 - xxx cym                                      
;;;; 09	:260 Elec Hat 4    : 042 - xxx cym                                      
;;;; 10	:045 HouseKick3    : 035 - xxx kick                                  
;;;; 11	:                  : xxx - xxx                                     
;;;; 12	:                  : xxx - xxx                                       
;;;; 13	:                  : xxx - xxx                                   
;;;; 14	:                  : xxx - xxx                                    
;;;; 15	:302 DarkCymbal1   : 044 - xxx cym                                      
;;;; 16 :303 DarkCymbal2   : 046 - xxx cym                                    
;;;; 17	:                  : xxx - xxx                                       
;;;; 18	:                  : xxx - xxx                                       
;;;; 19	:                  : xxx - xxx                                    
;;;; 20	:                  : xxx - xxx
;;;; 21	:285 Bckwrd Hat    : 047 - xxx cym                                       
;;;; 22	:                  : xxx - xxx                                        
;;;; 23	:223 AcousRoto1    : 072 - 122                                       
;;;; 24	:224 ElecTomic     : 048 - 072                                       
;;;;

(in-package :cyco)

(param rocket-drums nil)
(let ((kick-map (keymap 'rocket-kick '((A 35)(B 36)(C 38))))
      (snare-map (keymap 'rocket-snare '((A 40)(B 41)(C 43)(D 45))))
      (cym-map (circular-keymap 'rocket-cymbals '(37 39 42 44 46 47)))
      (tom1-map (circular-keymap 'rocket-toms1 (range 48 73)))
      (tom2-map (circular-keymap 'rocket-toms2 (range 72 123)))
      (program-number (car (cdr (assoc 'latin-drums +PROCUSSION-PROGRAMS+)))) )

  (defun rocket-drums (&key (parent pro3))
    (setf rocket-drums (create-instrument 'rocket-drums
					  :parent parent
					  :transient t
					  :program-change-hook
					  (constant-program-hook
					   'rocket-drums program-number)))
    (param rocket-kick (create-instrument 'rocket-kick
					  :parent rocket-drums
					  :keynumber-map kick-map))
    (param rocket-snare (create-instrument 'rocket-snare
					   :parent rocket-drums
					   :keynumber-map snare-map))
    (param rocket-cym (create-instrument 'rocket-cym
					 :parent rocket-drums
					 :keynumber-map cym-map))
    (param rocket-tom1 (create-instrument 'rocket-tom1
					  :parent rocket-drums
					  :keynumber-map tom1-map))
    (param rocket-tom2 (create-instrument 'rocket-tom2
					  :parent rocket-drums
					  :keynumber-map tom2-map))
    rocket-drums))
