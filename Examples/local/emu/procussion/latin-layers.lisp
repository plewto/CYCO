;;;; emu procussion Latin-Layers
;;;;
;;;; Zone Stack           :  key-range
;;;;  1   399 timbale1b   :  36 -  47                                                             
;;;;  2   368 tambourine  :  36 -  53                                                          
;;;;  3   351 hand drum   :  48 -  59                                                         
;;;;  4   396 shaker back :  54 -  65                                                           
;;;;  5   365 clave       :  60 -  71                                                     
;;;;  6   392 guiro down  :  66 -  77                                                          
;;;;  7   404 tumba tone  :  72 -  74                                                       
;;;;  8   394 shaker net  :  78 -  96                                                       
;;;;  9   405 tumba rim   :  75 -  77                                                      
;;;; 10   406 quinto tone :  78 -  80                                                            
;;;; 11   407 quintoSlpO  :  81 -  83                                                          
;;;; 12   408 quintoSlpC  :  84 -  86                                                          
;;;; 13   413 Macho Tip L :  87 -  89                                                           
;;;; 14   414 Macho Tip R :  90 -  92
;;;; 15   233 HiHatA 1/3  :  98 -  98                                                       
;;;; 16   233 HiHatA 1/3  :  62 -  62
;;;; 17   414 Macho Tip R : 100 - 100                                             
;;;; 18   416 Macho Tone  : 101 - 101                                             
;;;; 19   341 RevSnapper  : 103 - 103                                            
;;;; 20                   :                                              
;;;; 21                   :                                              
;;;; 22                   :                                              
;;;; 23                   :                                              
;;;; 24                   :                                              

(in-package :cyco)

(param latin-layers nil)
(let ((timbale-map (circular-keymap 'timbale (range 36 47)))
      (tambourine-map (circular-keymap 'tambourine (range 36 53)))
      (hand-drum-map (circular-keymap 'hand-drum (range 48 59)))
      (shaker-map (circular-keymap 'shaker (zip (range 54 65)(range 78 96))))
      (guiro-map (circular-keymap 'guiro (range 66 77)))
      (tumba-map (keymap 'tumba '((a 72)(rim-a 75)
				  (b 73)(rim-b 76)
				  (c 74)(rim-c 77))))
      (quinto-map (keymap 'quinto '((a 78)(slap-a1 81)(slap-a2 84)
				     (b 79)(slap-b1 82)(slap-b2 85)
				     (c 80)(slap-c1 83)(slap-c2 86))))
      (macho-map (keymap 'macho '((a 87)(b 90)(c 100)(d 101)(a2 88)(b2 91)(a3 89)(b3 92))))
      (hat-map (circular-keymap 'hat '(98 62 103)))
      (program-number (car (cdr (assoc 'latin-layers +PROCUSSION-PROGRAMS+)))) )
  (defun latin-layers (&key (parent pro3))
    (setf latin-layers (create-instrument 'latin-layers
					  :parent parent
					  :transient t
					  :program-change-hook
					  (constant-program-hook
					   'latin-layers program-number)))
    (param ll-timbale (create-instrument 'll-timbale
					 :parent latin-layers
					 :keynumber-map timbale-map))
    (param ll-tambourine (create-instrument 'll-tambourine
					    :parent latin-layers
					    :keynumber-map tambourine-map))
    (param ll-hand-drum (create-instrument 'll-hand-drum
					   :parent latin-layers
					   :keynumber-map hand-drum-map))
    (param ll-shaker (create-instrument 'll-shaker
					:parent latin-layers
					:keynumber-map shaker-map))
    (param ll-guiro (create-instrument 'll-guiro
				       :parent latin-layers
				       :keynumber-map guiro-map))
    (param ll-tumba (create-instrument 'll-tumba
				       :parent latin-layers
				       :keynumber-map tumba-map))
    (param ll-quinto (create-instrument 'll-quinto
					:parent latin-layers
					:keynumber-map quinto-map))
    (param ll-macho (create-instrument 'll-macho
				       :parent latin-layers
				       :keynumber-map macho-map))
    (param ll-hat (create-instrument 'll-hat
				     :parent latin-layers
				     :keynumber-map hat-map))
    latin-layers))
      

