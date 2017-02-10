;;;; cyco/Local/Emu/Procussion/rocket-drums.lisp
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

(defun --rd-kick-map (k)
  (cond ((numberp k)
	 (cnth (truncate k) '(35 36 38)))
	((eq k '?)(format t "0 1 2~%") 0)
	((and k (symbolp k))(--rd-kick-map (keynum k)))
	(t 0)))

(defun --rd-snare-map(k)
  (cond ((numberp k)
	 (cnth (truncate k) '(40 41 43 45)))
	((eq k '?)(format t "0 1 2 3~%") 0)
	((and k (symbolp k))(--rd-snare-map (keynum k)))
	(t 0)))

(defun --rd-cym-map(k)
  (cond ((numberp k)
	 (cnth (truncate k) '(37 39 42 44 46 47)))
	((eq k '?)(format t "0 1 2 3 4 5~%") 0)
	((and k (symbolp k))(--rd-cym-map (keynum k)))
	(t 0)))

(constant +rocket-drums-tom1-keynums+ (range-between 48 73))
(constant +rocket-drums-tom2-keynums+ (range-between 72 123))

(defun --rd-tom1-map (k)
  (cond ((numberp k)
	 (cnth (truncate k) +rocket-drums-tom1-keynums+))
	((eq k '?)
	 (format t "0 - ~A~%" (length +rocket-drums-tom1-keynums+)) 0)
	((and k (symbolp k))(--rd-tom1-map (keynum k)))
	(t 0)))

(defun --rd-tom2-map (k)
  (cond ((numberp k)
	 (cnth (truncate k) +rocket-drums-tom2-keynums+))
	((eq k '?)
	 (format t "0 - ~A~%" (length +rocket-drums-tom2-keynums+)) 0)
	((and k (symbolp k))(--rd-tom2-map (keynum k)))
	(t 0)))

(param rocket-drums nil)

(defun rocket-drums (&optional (parent pro3))
  (remove-children parent)
  (set-value parent :program (procussion-program-map 'rocket-drums))
  (let* ((rd (instrument :rocket-drums
			 :parent parent))
	 (kk (instrument :rd-kick
			 :parent rd
			 :keymap #'--rd-kick-map))
	 (sn (instrument :rd-snare
			 :parent rd
			 :keymap #'--rd-snare-map))
	 (cy (instrument :rd-cymbal
			 :parent rd
			 :keymap #'--rd-cym-map))
	 (tm (instrument :rd-tomic
			 :parent rd
			 :keymap #'--rd-tom1-map))
	 (rt (instrument :rd-roto
			 :parent rd
			 :keymap #'--rd-tom2-map)))
    (setf rocket-drums rd)
    (param rd-kick kk)
    (param rd-snare sn)
    (param rd-cymbal cy)
    (param rd-tomic tm)
    (param rd-roto rt)
    rd))
