;;;; emu procussion more-toms
;;;;
;;;; Zone Stack
;;;;  1 201 ResonanTom    : 036 043 A                                                             
;;;;  2 214 Apache        : 042 049 B                                                          
;;;;  3 183 SloppyToms    : 048 055 C                                                         
;;;;  4 196 PowerToms     : 054 061 D                                                           
;;;;  5 221 Pitch Toms    : 060 067 E                          
;;;;  6 215 Tacky Bang    : 066 073 F                                                          
;;;;  7 216 Pots & Pans   : 070 073 G                                                       
;;;;  8                   :                                                                 
;;;;  9                   :                                                                
;;;; 10 188 Room Tom 1    : 079 083                                                              
;;;; 11 230 ModBrshTom    : 084 088                                                            
;;;; 12 229 ModWundTom    : 089 093                                                            
;;;; 13 228 ModTomFall    : 094 098                                                             
;;;; 14                   :                                       
;;;; 15 198 PowerTom 4    : 074 078                                                         
;;;; 16 231 ModEchoTom    : 079 083                                  
;;;; 17                   :                                                       
;;;; 18                   :                                                       
;;;; 19                   :                                                      
;;;; 20                   :                                              
;;;; 21                   :                                              
;;;; 22                   :                                              
;;;; 23                   :                                              
;;;; 24                   :         
;;;;

(in-package :cyco)

(let*((program-number (car (cdr (assoc 'more-toms +PROCUSSION-PROGRAMS+))))
      (acc '())
      (bcc '()))
  (dotimes (counter 6)
    (dolist (spec '((A 36 "ResonanTom")    ; 36 37 38 39 40 41 
		    (B 42 "Apache")        ; 42 43 44 45 46 47 
		    (C 48 "SloppyToms")    ; 48 49 50 51 52 53 
		    (D 54 "PowerToms")     ; 54 55 56 57 58 59 
		    (E 60 "Pitch Toms")    ; 60 61 62 63 64 65 
		    (F 66 "Tacky Bang")))  ; 66 67 68 69 70 71 72 73
      (let ((name (if (zerop counter)
		      (car spec)
		    (intern (str+ (car spec) counter))))
	    (value (+ counter (second spec))))
	(push (list name value (third spec)) acc))))
  (push '(F6 72 "Tacky Bang") acc)
  (push '(F7 73 "Tacky Bang") acc)
  (dotimes (counter 5)
    (dolist (spec '((G 74 "Power Tom 4")
		    (H 79 "Room Tom 1")
		    (I 84 "ModBrshTom")
		    (J 89 "ModWundTom")
		    (K 94 "ModTomFall")))
      (let ((name (if (zerop counter)
		      (car spec)
		    (intern (str+ (car spec) counter))))
	    (value (+ counter (second spec))))
	(push (list name value (third spec)) bcc))))
  (setf acc (append (reverse acc)(reverse bcc)))
  (param more-toms nil)
  (defun more-toms (&key (parent pro3))
    (setf more-toms (create-instrument 'more-toms
					 :parent parent
					 :transient t
					 :program-change-hook
					 (constant-program-hook
					  'more-toms program-number)
					 :keynumber-map
					 (keymap
					  'more-toms acc)))
    more-toms))
  
