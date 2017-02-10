;;;; cyco emu procussion proshake
;;;;
;;;; Instruments
;;;;    proshake
;;;;        tambourine
;;;;        cabasa
;;;;        shaker
;;;;        proshake-noyz
;;;;

(in-package :cyco)

(defkeymap --proshake-tambourine-map '((TAM   . 025)
				       (SHK   . 028)
				       (NSE   . 031)
				       (NHT   . 034)
				       (SHT   . 037)
				       (HHT   . 040)
				       (THT   . 043)
				       (CAST  . 046)
				       (TAM2  . 024)
				       (SHK2  . 027)
				       (NSE2  . 030)
				       (NHt2  . 033)
				       (SHT2  . 036)
				       (HHT2  . 039)
				       (THT2  . 042)
				       (CAST2 . 045)
				       (TAM3  . 026)
				       (SHK3  . 029)
				       (NSE3  . 032)
				       (NHT3  . 035)
				       (SHT3  . 039)
				       (HHT3  . 041)
				       (THT3  . 044)
				       (CAST3 . 047)))


(defkeymap --proshake-cabasa-map '((FRNT   . 049)
				   (BACK   . 052)
				   (ROLL   . 055)
				   (GUIRO  . 057)
				   (GUIRO2 . 058)
				   (GUIRO3 . 059)
				   (FRNT2  . 048)
				   (BACK2  . 053)
				   (ROLL2  . 054)
				   (FRNT3  . 050)
				   (BACK3  . 053)
				   (ROLL   . 056)))


(defkeymap --proshake-shaker-map '((NET      . 061)
				   (SNAP     . 064)
				   (BACK     . 067)
				   (FRONT    . 070)
				   (SHAKE    . 073)
				   (GUIRO    . 076)
				   (SWEEP    . 079)
				   (JANG     . 082)
				   (NET2     . 060)
				   (SNAP2    . 063)
				   (BACK2    . 066)
				   (FRONT2   . 069)
				   (SHAKE2   . 072)
				   (GUIRO2   . 075)
				   (SWEEP2   . 078)
				   (JANG2    . 081)
				   (NET3     . 062)
				   (SNAP3    . 065)
				   (BACK3    . 068)
				   (FRONT3   . 071)
				   (SHAKE3   . 074)
				   (GUIRO3   . 077)
				   (SWEEP3   . 080)
				   (JANG3    . 083)
				   (CRACK    . 085)
				   (CRACK2   . 084)
				   (CRACK3   . 086)))

(circular-keymap --proshake-noyz-map (range 40 127))

(param proshake nil)
(param tambourine nil)
(param cabasa nil)
(param shaker nil)
(param proshake-noyz nil)


(defun proshake (&key (parent pro3))
  (let ((prgnm (car (cdr (assoc 'proshake +PROCUSSION-PROGRAMS+)))))
    (setf proshake (create-instrument
		    'proshake
		    :parent parent
		    :transient t
		    :program-change-hook (constant-program-hook
					  'proshake prgnm)))
    (setf tambourine (create-instrument
		      'tambourine
		      :transient t
		      :parent proshake
		      :keynumber-map #'--proshake-tambourine-map))
    (setf cabasa (create-instrument
		  'cabasa
		  :transient t
		  :parent proshake
		  :keynumber-map #'--proshake-cabasa-map))
    (setf shaker (create-instrument
		  'shaker
		  :transient t
		  :parent proshake
		  :keynumber-map #'--proshake-shaker-map))
    (setf proshake-noyz (create-instrument
			 'proshake-noyz
			 :transient t
			 :parent proshake
			 :keynumber-map #'--proshake-noyz-map))
    proshake))
