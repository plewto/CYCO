;;;; emu procussion orch-percussion
;;;;
;;;; Zone Stack
;;;;  1 071 Timpani       : 053 072                                                               
;;;;  2 067 Orch Bass     : 036 036                                                            
;;;;  3 080 BrushSnr A    : 040 040                                                           
;;;;  4 081 BrushSnr B    : 041 041                                                             
;;;;  5 082 BrushSnr C    : 043 043                                                       
;;;;  6 090 Room Snr 1    : 038 038                                                            
;;;;  7 100 TonalSnare    : 039 039                                                         
;;;;  8 103 Tite Snare    : 047 047                                                         
;;;;  9 091 Room Snr 2    : 045 045                                                        
;;;; 10                   :                                                                      
;;;; 11                   :                                                                    
;;;; 12 073 KettleDrm2    : 048 052                                                            
;;;; 13                   :                                                                     
;;;; 14                   :                                       
;;;; 15                   :                                                                 
;;;; 16                   :                                          
;;;; 17                   :                                                       
;;;; 18                   :                                                       
;;;; 19 293 China Gong    : 042 042                                              
;;;; 20 368 Tambourine    : 037 037                                      
;;;; 21 308 FingerCymb    : 084 096                                      
;;;; 22 292 MallCymbal    : 046 046                                      
;;;; 23 294 MalletRoll    : 044 044                                      
;;;; 24 362 Triangle      : 073 084

(in-package :cyco)

(let ((timpani-map (circular-keymap 'timpani (range 53 72)))
      (bass-map (circular-keymap 'bass '(36)))
      (snare-map (keymap 'snare '((brush-a 40)(brush-b 41)(brush-c 43)
				  (room-1 38)(room-2 45)(tonal 39)(tite 47))))
      (kettle-map (circular-keymap 'kettle-drum (range 48 52)))
      (cymbal-map (keymap 'cymbal '((china 42)(tambourine 37)(mallet 46)(roll 44))))
      (tambourine-map (circular-keymap 'tambourine '((37))))
      (triangle-map (circular-keymap 'triangle (range 73 84)))
      (program-number (car (cdr (assoc 'orch-percussion +PROCUSSION-PROGRAMS+)))) )
  (param orch-percussion nil)

  (defun orch-percussion (&key (parent pro3))
    (setf orch-percussion (create-instrument 'orch-percussion
					     :parent parent
					     :transient nil
					     :program-change-hook
					     (constant-program-hook
					      'orch-percussion program-number)))
    (param op-timpani (create-instrument 'op-timpani
					 :parent orch-percussion
					 :keynumber-map timpani-map))
    (param op-bass (create-instrument 'op-bass
				      :parent orch-percussion
				      :keynumber-map bass-map))
    (param op-snare (create-instrument 'op-snare
				       :parent orch-percussion
				       :keynumber-map snare-map))
    (param op-kettle (create-instrument 'op-kettle
					:parent orch-percussion
					:keynumber-map kettle-map))
    (param op-cymbal (create-instrument 'op-cymbal
					:parent orch-percussion
					:keynumber-map cymbal-map))
    (param op-tambourine (create-instrument 'op-tambourine
					    :parent orch-percussion
					    :keynumber-map tambourine-map))
    (param op-triangle (create-instrument 'op-triangle
					  :parent orch-percussion
					  :keynumber-map triangle-map))
    orch-percussion))
		   
       

