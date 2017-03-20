;;;; cyco emu procussion discoball
;;;;
;;;; Instruments:
;;;;    discoball
;;;;        discoball-kick
;;;;        discoball-snare
;;;;        discoball-hat
;;;;        discoball-bass
;;;;
;;;; Zone    Stack               Key         Tune
;;;; 01      :022 BigFoot        :036-036    :                               
;;;; 02      :049 CymbalKick     :037-037    :
;;;; 03      :014 Slap-Snare     :038-038    :                                  
;;;; 04      :154 Trash-snare-1  :041-041    :                                     
;;;; 05      :155 Trash-snare-4  :043-043    :                                     
;;;; 06      :143 HouseSnare-1   :045-045    :                                    
;;;; 07      :265 HouseHat-3     :042-042    :                                  
;;;; 08      :336 Syn-Scrtch     :044-044    :                                  
;;;; 09      :272 Horny-hat      :046-046    :                                 
;;;; 10      :368 Tambourine     :049-049    :                                  
;;;; 11      :157 Trash-snare-4  :098-098    :                                     
;;;; 12      :333 rap-scratch    :100-100    :                                   
;;;; 13      
;;;; 14
;;;; 15
;;;: 16
;;;; 17
;;;; 18     :022 BigFoot         :050-060    :                               
;;;; 19     :343 HandClaps       :039-039    :                                
;;;; 20     :429 BASS-STACK      :072-096    :                                 
;;;; 21     :147 HOUSESNR-5      :040-040    :                                 
;;;; 22     :066 TRASH-KICK      :047-047    :                                 
;;;; 23     :164 CLAVESNARE      :048-048    :                                 
;;;; 24     :104 SLAP-SNARE      :061-071    :                  
;;;;

(in-package :cyco)

(param discoball nil)

(let ((kick-map (keymap 'disco-kick '((BIG   . 036)
				      (CYM   . 037)
				      (TRASH . 047)
				      (BIG0  . 050)
				      (BIG1  . 061)
				      (BIG2  . 062)
				      (BIG3  . 063)
				      (BIG4  . 064)
				      (BIG5  . 065)
				      (BIG6  . 066)
				      (BIG7  . 067)
				      (BIG8  . 068)
				      (BIG9  . 069)
				      (BIG10 . 070))))
      (snare-map (keymap 'disco-snare '((SLAP    . 038)
					(HOUSE   . 040)
					(HOUSE2  . 045)
					(TRASH   . 041)
					(TRASH1  . 043)
					(TRASH2  . 098)
					(CLAVE   . 048)
					(SLAP0   . 061)
					(SLAP1   . 062)
					(SLAP2   . 063)
					(SLAP3   . 064)
					(SLAP4   . 065)
					(SLAP5   . 066)
					(SLAP6   . 067)
					(SLAP7   . 068)
					(SLAP8   . 069)
					(SLAP9   . 070)
					(SLAP10  . 071))))
      (hat-map (keymap 'disco-hat '((HOUSE  . 042)
				    (HORNY  . 043)
				    (CLAP   . 039)
				    (TAMB   . 049)
				    (SCRATCH . 044)
				    (scratch2 . 100))))
      (bass-map (reduced-keymap 'disco-bass 72 96))
      (program-number (car (cdr (assoc 'discoball +PROCUSSION-PROGRAMS+)))) )
  (setf discoball nil)

  (defun discoball (&key (parent pro3))
    (setf discoball (create-instrument 'discoball
				       :parent parent
				       :transient t
				       :program-change-hook (constant-program-hook
							     'discoball program-number)))
    (param disco-kick (create-instrument 'disco-kick
					:parent discoball
					:keynumber-map kick-map))
    (param disco-snare (create-instrument 'disco-snare
					  :parent discoball
					  :keynumber-map snare-map))
    (param disco-hat (create-instrument 'disco-hat
					:parent discoball
					:keynumber-map hat-map))
    (param disco-bass (create-instrument 'disco-bass
					 :parent discoball
					 :keynumber-map bass-map))
    discoball)) 

