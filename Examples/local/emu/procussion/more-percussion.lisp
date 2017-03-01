;;;; emu procussion more-percussion
;;;;
;;;; Zone Stack
;;;;  1 420 Conga Set     : 036 048              
;;;;  2 496 Klinkle       : 048 060              
;;;;  3 497 Pipetam       : 060 072              
;;;;  4 498 Bellism       : 072 084              
;;;;  5 499 Jangler       : 084 096              
;;;;  6                   :                      
;;;;  7                   :                      
;;;;  8                   :                      
;;;;  9                   :                      
;;;; 10                   :                      
;;;; 11                   :                      
;;;; 12                   :                      
;;;; 13                   :                      
;;;; 14                   :                      
;;;; 15                   :                      
;;;; 16                   :                      
;;;; 17                   :                      
;;;; 18                   :                      
;;;; 19                   :                      
;;;; 20                   :                      
;;;; 21                   :                      
;;;; 22                   :                      
;;;; 23                   :                      
;;;; 24                   :                      
;;;;

(let ((program-number (car (cdr (assoc 'more-percussion +PROCUSSION-PROGRAMS+)))))
  (param more-percussion nil)
  (defun more-percussion (&key (parent pro3))
    (setf more-percussion (create-instrument 'more-percussion
					 :parent parent
					 :transient t
					 :program-change-hook
					 (constant-program-hook
					  'more-percussion program-number)))
    (param mp-conga (create-instrument 'mp-conga
				       :parent more-percussion
				       :keynumber-map
				       (reduced-keymap 'mp-conga 36 48)))
    (param mp-klinkle (create-instrument 'mp-klinkle
					 :parent more-percussion
					 :keynumber-map
					 (reduced-keymap 'mp-klinkle 48 60)))
    (param mp-pipetam (create-instrument 'mp-pipetam
					 :parent more-percussion
					 :keynumber-map
					 (reduced-keymap 'mp-pipetam 60 72)))
    (param mp-bellism (create-instrument 'mp-bellism
					 :parent more-percussion
					 :keynumber-map
					 (reduced-keymap 'mp-bellism 72 84)))
    (param mp-jangler (create-instrument 'mp-jangler
					 :parent more-percussion
					 :keynumber-map
					 (reduced-keymap 'mp-jangler 84 96)))
    more-percussion))
  
