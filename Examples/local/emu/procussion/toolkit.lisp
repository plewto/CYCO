;;;; emu procussion toolkit
;;;; 2012.09.19
;;;; Instruments
;;;;    toolkit
;;;;        tk-plank
;;;;        tk-ratchet
;;;;        tk-awkbosk
;;;;        tk-gymnasium
;;;;        tk-snare
;;;;        tk-cymbal
;;;;        tk-twank
;;;;        tk-waterpin
;;;;        tk-glong
;;;;        tk-temple
;;;;        tk-clank
;;;;        tk-echothud
;;;;        tk-spacekick
;;;;        tk-tom
;;;; 
;;;; Zone    Stack             key         Tune
;;;; 01     :540 Twank        :036 038    :+00+00
;;;; 02     :541 MegaPlanks   :039 041    :+00+00    
;;;; 03     :514 Water-Pin    :042 044    :-36+00
;;;; 04     :542 TemploDoom   :045 047    :+00+00
;;;; 05     :543 Glong        :048 050    :+00+00
;;;; 06     :544 GiantRachet  :051 053    :+00+00
;;;; 07     :367 Wood-Block   :054 056 *  :-24+00
;;;; 08     :330 Clank        :054 056 *  :+00+00
;;;; 09     :545 Echo-Thud    :057 059    :+00+00
;;;; 10     :546 Awk-Bosk     :060 062    :+00+00
;;;; 11     :547 Gymnasium    :063 065    :+00+00     
;;;; 12     :544 GiantRachet  :066 068    :-36+00
;;;; 13     :541 MegaPlanks   :069 071    :+48+00
;;;; 14     :547 Gymnasium    :072 074    :+48+00
;;;; 15     :546 Awk-Bosk     :075 077    :+48+00
;;;; 16     :057 Space-Kick   :078 080    :-12+00
;;;; 17     :124 PrariSnare   :081 083    :+12+00
;;;; 18     :187 16-Choke-1   :084 086    :+12+00
;;;; 19     :318 Hyperreal    :087 089    :-12+00
;;;; 20     :115 Ambi-snr-1   :090 092    :+24+00
;;;; 21     :118 ModVerbSnr   :093 095    :+24+00
;;;; 22     :171 RevSnare-2   :096 098    :+24+00
;;;; 23     :125 SNARE-1157   :099 101    :+24+00
;;;; 24     :209 TunedTomz    :102 104    :+24+00
;;;;

(in-package :cyco)

(param toolkit nil)

(let ((plank-map (keymap 'plank '((A 039)
				  (A2 040)
				  (A3 041)
				  (B 069)
				  (B1 070)
				  (B2 071))))
      (rachet-map (keymap 'rachet '((A 051)
				    (A2 052)
				    (A3 053)
				    (B 066)
				    (B2 067)
				    (B3 068))))
      (awkbosk-map (keymap 'awkbosk '((A 060)
				      (A1 061)
				      (A2 062)
				      (B 075)
				      (B1 076)
				      (B2 077))))
      (gymnasium-map (keymap 'gymnasium '((A 063)
					  (A1 064)
					  (A2 065)
					  (B 072)
					  (B1 073)
					  (B2 074))))
      (snare-map (keymap 'snare '((A  081)
				  (A2  082)
				  (A3  083)
				  (B  090)
				  (B2  091)
				  (B3  092)
				  (C  093)
				  (C1  094)
				  (C2  095)
				  (D  096)
				  (D2  097)
				  (D3  098)
				  (E  099)
				  (E2  100)
				  (E3  101))))
      (cymbal-map (keymap 'cymbal '((A  087)
				    (A2 088)
				    (A3 089)
				    (B  084)
				    (B2 085)
				    (B3 086))))
      (twank-map (keymap 'twank '((A 036)
				  (A2 037)
				  (Z3 038))))
      (waterpin-map (keymap 'waterpin '((A 042)
					(A1 043)
					(A2 044))))
      (glong-map (keymap 'glong '((A 048)
				  (A2 049)
				  (A3 050))))
      (temple-map (keymap 'temple '((A  045)
				    (A1 046)
				    (A2 047))))
      (clank-map (keymap 'clank '((A 054)
				  (A2 055)
				  (A3 056))))
      (echothud-map (keymap 'echothud '((A 057)
					(A2 058)
					(A3 059))))
      (spacekick-map (keymap 'spacekick '((A 078)
					  (A2 079)
					  (A3 080))))
      (tom-map (keymap 'tom '((A 102)
			      (A2 103)
			      (A3 104))))
      (program-number (car (cdr (assoc 'toolkit +PROCUSSION-PROGRAMS+)))) )

  (setf toolkit nil)

  (defun toolkit (&key (parent pro3))
    (setf toolkit (create-instrument 'toolkit
				     :parent parent
				     :transient t
				     :program-change-hook (constant-program-hook
							   'toolkit program-number)))
    (param tk-plank (create-instrument 'tk-plank
				       :parent toolkit
				       :keynumber-map plank-map))
    (param tk-rachet (create-instrument 'tk-rachet
					:parent toolkit
					:keynumber-map rachet-map))
    (param tk-awkbosk (create-instrument 'tk-awkbosk
					 :parent toolkit
					 :keynumber-map awkbosk-map))
    (param tk-gymnasium (create-instrument 'tk-gymnasium
					   :parent toolkit
					   :keynumber-map gymnasium-map))
    (param tk-snare (create-instrument 'tk-snare
				       :parent toolkit
				       :keynumber-map snare-map))
    (param tk-cymbal (create-instrument 'tk-cymbal
					:parent toolkit
					:keynumber-map cymbal-map))
    (param tk-twank (create-instrument 'tk-twank
				       :parent toolkit
				       :keynumber-map twank-map))
    (param tk-waterpin (create-instrument 'tk-waterpin
					  :parent toolkit
					  :keynumber-map waterpin-map))
    (param tk-glong (create-instrument 'tk-glong
				       :parent toolkit
				       :keynumber-map glong-map))
    (param tk-temple (create-instrument 'tk-temple
					:parent toolkit
					:keynumber-map temple-map))
    (param tk-clank (create-instrument 'tk-clank
				       :parent toolkit
				       :keynumber-map clank-map))
    (param tk-echothud (create-instrument 'tk-echothud
					  :parent toolkit
					  :keynumber-map echothud-map))
    (param tk-spacekick (create-instrument 'tk-spacekick
					   :parent toolkit
					   :keynumber-map spacekick-map))
    (param tk-tom (create-instrument 'tk-tom
				     :parent toolkit
				     :keynumber-map tom-map))
    toolkit))
    
    
    
