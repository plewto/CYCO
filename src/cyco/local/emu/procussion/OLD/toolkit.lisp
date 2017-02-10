;;;; cyco/Local/Emu/Procussion/toolkit.lisp
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

(defmap --tk-plank-map '((A  . 039)
			 (A2 . 040)
			 (A3 . 041)
			 (B  . 069)
			 (B1 . 070)
			 (B2 . 071)))

(defmap --tk-rachet-map '((A  . 051)
			  (A2 . 052)
			  (A3 . 053)
			  (B  . 066)
			  (B2 . 067)
			  (B3 . 068)))

(defmap --tk-awkbosk-map '((A  . 060)
			   (A1 . 061)
			   (A2 . 062)
			   (B  . 075)
			   (B1 . 076)
			   (B2 . 077)))

(defmap --tk-gymnasium-map '((A  . 063)
			     (A1 . 064)
			     (A2 . 065)
			     (B  . 072)
			     (B1 . 073)
			     (B2 . 074)))

(defmap --tk-snare-map  '((A   .  081)
			  (A2  .  082)
			  (A3  .  083)
			  (B   .  090)
			  (B2  .  091)
			  (B3  .  092)
			  (C   .  093)
			  (C1  .  094)
			  (C2  .  095)
			  (D   .  096)
			  (D2  .  097)
			  (D3  .  098)
			  (E   .  099)
			  (E2  .  100)
			  (E3  .  101)))

(defmap --tk-cymbal-map '((A   . 087)
			  (A2  . 088)
			  (A3  . 089)
			  (B   . 084)
			  (B2  . 085)
			  (B3  . 086)))

(defmap --tk-twank-map '((A  . 036)
			 (A2 . 037)
			 (Z3 . 038)))

(defmap --tk-waterpin-map  '((A  . 042)
			     (A1 . 043)
			     (A2 . 044)))

(defmap --tk-glong-map '((A  . 048)
			 (A2 . 049)
			 (A3 . 050)))

(defmap --tk-temple-map '((A   . 045)
			  (A1  . 046)
			  (A2  . 047)))

(defmap --tk-clank-map '((A  . 054)
			 (A2 . 055)
			 (A3 . 056)))

(defmap --tk-echothud-map '((A  . 057)
			    (A2 . 058)
			    (A3  . 059)))

(defmap --tk-spacekick-map '((A  . 078)
			     (A2 . 079)
			     (A3 . 080)))

(defmap --tk-tom-map '((A  . 102)
		       (A2 . 103)
		       (A3 . 104)))
(param toolkit nil)

(defun toolkit (&optional (parent pro4))
  (remove-children parent)
  (set-value parent :program (procussion-program-map 'toolkit))
  (let ((tk (instrument 'toolkit
			:parent parent)))
    (param tk-plank (instrument :tk-plank
				:parent tk
				:keymap #'--tk-plank-map))
    (param tk-rachet (instrument :tk-rachet
				 :parent tk
				 :keymap #'--tk-rachet-map))
    (param tk-awkbosk (instrument :tk-awkbosk
				  :parent tk
				  :keymap #'--tk-awkbosk-map))
    (param tk-gymnasium (instrument :tk-gymnasium
				    :parent tk
				    :keymap #'--tk-gymnasium-map))
    (param tk-snare (instrument :tk-snare
				:parent tk
				:keymap #'--tk-snare-map))
    (param tk-cymbal (instrument :tk-cymbal
				 :parent tk
				 :keymap #'--tk-cymbal-map))
    (param tk-twank (instrument :tk-twank
				:parent tk
				:keymap #'--tk-twank-map))
    (param tk-waterpin (instrument :tk-waterpin
				   :parent tk
				   :keymap #'--tk-waterpin-map))
    (param tk-glong (instrument :tk-glong
				:parent tk
				:keymap #'--tk-glong-map))
    (param tk-temple (instrument :tk-temple
				 :parent tk
				 :keymap #'--tk-temple-map))
    (param tk-clank (instrument :tk-clank
				:parent tk
				:keymap #'--tk-clank-map))
    (param tk-echothud (instrument :tk-echothud
				   :parent tk
				   :keymap #'--tk-echothud-map))
    (param tk-spacekick (instrument :tk-spacekick
				    :parent tk
				    :keymap #'--tk-spacekick-map))
    (param tk-tom (instrument :tk-tom
			      :parent tk
			      :keymap #'--tk-tom-map))
    (setf toolkit tk)
    tk))
