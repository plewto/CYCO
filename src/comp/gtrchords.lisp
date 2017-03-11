;;;; cyco comp.gtrchords
;;;;
;;;; Defines chords limited to possible guitar chords.
;;;; For each pitch-class there are several "families" (:MAJ :MIN :MAJ7 etc...)
;;;; Each family has a number of variations.   In general variations which
;;;; appear near the start of each family list are located lower on the
;;;; guitar neck.
;;;;
;;;;  Assumes standard guitar tuning  E A D G B E.
;;;;

(in-package :cyco)

(constant +GTR-CHORD-FAMILIES+
	  '(:SOLO :MAJ :MAJ7 :DOM7 :MAJ9 :MAJ11 
		  :MIN :MIN7 :MIN9 :MIN11 :DIM 
		  :6TH :MIN6 :SUS4 :SUS2))

(constant  +GTR-CHORDS+
	   '#(((:SOLO (60) (72) (84))
	       (:MAJ (55 60 67 72 64 79) 
		     (60 64 67 72 64) 
		     (60 67 72) 
		     (60 67 72 76 79 84)
		     (64 72 79 84) 
		     (67 72 79 84 88) 
		     (72 79 84))
	       (:MAJ7 (55 60 67 71 64 79) 
		      (60 64 67 71 64) 
		      (60 67 71) 
		      (60 67 72 76 79 83)
		      (64 72 79 83) 
		      (67 72 79 83 88) 
		      (72 79 83))
	       (:DOM7 (55 60 67 70 64 79) 
		      (60 64 67 70 64) 
		      (60 67 70) 
		      (60 67 72 76 79 82)
		      (64 72 79 82) 
		      (67 72 79 82 88) 
		      (72 79 82))
	       (:MAJ9 (55 60 64 67 74) 
		      (62 67 71 76) 
		      (62 67 72 76 83) 
		      (67 72 76 83 86)
		      (72 67 74 67 83 88) 
		      (83 76 79 74))
	       (:MAJ11 (55 60 65 70 74 79) 
		       (70 77 79 88) 
		       (72 65 70 76 79 86)
		       (62 67 72 77 82 88) 
		       (67 86 77 82) 
		       (74 79 82 89))
	       (:MIN (55 60 67 72 63 79) 
		     (60 63 67 72 63) 
		     (60 67 72) 
		     (60 67 72 75 79 84)
		     (63 72 79 84) 
		     (67 72 79 84 87) 
		     (72 79 84))
	       (:MIN7 (55 60 67 70 63 79) 
		      (60 63 67 70 63) 
		      (60 67 70) 
		      (60 67 72 75 79 82)
		      (63 72 79 82) 
		      (67 72 79 82 87) 
		      (72 79 82))
	       (:MIN9 (55 60 63 67 74) 
		      (62 67 70 75) 
		      (62 67 72 75 82) 
		      (67 72 75 82 86)
		      (72 67 74 67 82 87) 
		      (82 75 79 74))
	       (:MIN11 (55 60 65 70 74 79) 
		       (70 77 79 87) 
		       (72 65 70 75 79 86)
		       (62 67 72 77 82 87) 
		       (67 86 77 82) 
		       (74 79 82 89))
	       (:DIM (54 60 66 72 63 78) 
		     (60 63 66 72 63) 
		     (60 66 72) 
		     (60 66 72 75 78 84)
		     (63 72 78 84) 
		     (66 72 78 84 87) 
		     (72 78 84))
	       (:6TH (57 64 67 72 76 81) 
		     (60 69 76 79) 
		     (64 69 76 79 84 88) 
		     (64 67 72 79)
		     (67 72 76 81 84) 
		     (69 76 79 84) 
		     (72 79 81 88) 
		     (76 81 84 91))
	       (:MIN6 (57 63 67 72 75 81) 
		      (60 69 75 79) 
		      (63 69 75 79 84 87) 
		      (63 67 72 79)
		      (67 72 75 81 84) 
		      (69 75 79 84) 
		      (72 79 81 87) 
		      (75 81 84 91))
	       (:SUS4 (60 67 72 77 91 84) 
		      (60 67 72 89 79) 
		      (72 67 84 89) 
		      (72 77 79 84)
		      (72 77 91 84) 
		      (72 79 84 89))
	       (:SUS2 (60 67 72 86 79) 
		      (72 79 84 86)))
	      ((:SOLO (61) 
		      (73) 
		      (85))
	       (:MAJ (56 61 68 73 65 80) 
		     (61 65 68 73 65) 
		     (61 68 73) 
		     (61 68 73 77 80 85)
		     (65 73 80 85) 
		     (68 73 80 85 89) 
		     (73 80 85))
	       (:MAJ7 (56 61 68 72 65 80) 
		      (61 65 68 72 65) 
		      (61 68 72) 
		      (61 68 73 77 80 84)
		      (65 73 80 84) 
		      (68 73 80 84 89) 
		      (73 80 84))
	       (:DOM7 (56 61 68 71 65 80) 
		      (61 65 68 71 65) 
		      (61 68 71) 
		      (61 68 73 77 80 83)
		      (65 73 80 83) 
		      (68 73 80 83 89) 
		      (73 80 83))
	       (:MAJ9 (56 61 65 68 75) 
		      (63 68 72 77) 
		      (63 68 73 77 84) 
		      (68 73 77 84 87)
		      (73 68 75 68 84 89) 
		      (84 77 80 75))
	       (:MAJ11 (56 61 66 71 75 80) 
		       (71 78 80 89) 
		       (73 66 71 77 80 87)
		       (63 68 73 78 83 89) 
		       (68 87 78 83) 
		       (75 80 83 90))
	       (:MIN (56 61 68 73 64 80) 
		     (61 64 68 73 64) 
		     (61 68 73) 
		     (61 68 73 76 80 85)
		     (64 73 80 85) 
		     (68 73 80 85 88) 
		     (73 80 85))
	       (:MIN7 (56 61 68 71 64 80) 
		      (61 64 68 71 64) 
		      (61 68 71) 
		      (61 68 73 76 80 83)
		      (64 73 80 83) 
		      (68 73 80 83 88) 
		      (73 80 83))
	       (:MIN9 (56 61 64 68 75) 
		      (63 68 71 76) 
		      (63 68 73 76 83) 
		      (68 73 76 83 87)
		      (73 68 75 68 83 88) 
		      (83 76 80 75))
	       (:MIN11 (56 61 66 71 75 80) 
		       (71 78 80 88) 
		       (73 66 71 76 80 87)
		       (63 68 73 78 83 88) 
		       (68 87 78 83) 
		       (75 80 83 90))
	       (:DIM (55 61 67 73 64 79) 
		     (61 64 67 73 64) 
		     (61 67 73) 
		     (61 67 73 76 79 85)
		     (64 73 79 85) 
		     (67 73 79 85 88) 
		     (73 79 85))
	       (:6TH (58 65 68 73 77 82) 
		     (61 70 77 80) 
		     (65 70 77 80 85 89) 
		     (65 68 73 80)
		     (68 73 77 82 85) 
		     (70 77 80 85) 
		     (73 80 82 89) 
		     (77 82 85 80))
	       (:MIN6 (58 64 68 73 76 82) 
		      (61 70 76 80) 
		      (64 70 76 80 85 88) 
		      (64 68 73 80)
		      (68 73 76 82 85) 
		      (70 76 80 85) 
		      (73 80 82 88) 
		      (76 82 85 80))
	       (:SUS4 (61 68 73 78 80 85) 
		      (61 68 73 90 80) 
		      (73 68 85 90) 
		      (73 78 80 85)
		      (73 78 80 85) 
		      (73 80 85 90))
	       (:SUS2 (61 68 73 87 80) 
		      (73 80 85 87)))
	      ((:SOLO (50) 
		      (62) 
		      (74))
	       (:MAJ (57 62 69 74) 
		     (57 62 69 74 78) 
		     (57 62 69 74 78 81) 
		     (62 66 69 74 78)
		     (62 66 69 74 78 86) 
		     (62 69 74) 
		     (62 69 74 78 81) 
		     (62 69 74 78 81 86) 
		     (74 81)
		     (74 81 86))
	       (:MAJ7 (57 62 69 73) 
		      (57 62 69 73 78) 
		      (57 62 69 73 78 81) 
		      (62 66 69 73 78)
		      (62 66 69 74 78 85) 
		      (62 69 73) 
		      (62 69 73 78 81) 
		      (62 69 74 78 81 85) 
		      (73 81)
		      (74 81 85))
	       (:DOM7 (57 62 69 72) 
		      (57 62 69 72 78) 
		      (57 62 69 72 78 81) 
		      (62 66 69 72 78)
		      (62 66 69 74 78 84) 
		      (62 69 72) 
		      (62 69 72 78 81) 
		      (62 69 74 78 81 84) 
		      (72 81)
		      (74 81 84))
	       (:MAJ9 (57 66 73 76) 
		      (56 66 73 75) 
		      (64 69 73 78) 
		      (66 73 76 81) 
		      (66 76 81 85)
		      (69 76 78 84) 
		      (73 78 81 88))
	       (:MAJ11 (55 62 66 72 76 81) 
		       (62 67 72 78 81 88) 
		       (64 69 72 79) 
		       (67 72 76 81)
		       (72 79 81 88) 
		       (81 72 76 79))
	       (:MIN (57 62 69 74) 
		     (57 62 69 74 77) 
		     (57 62 69 74 77 81) 
		     (62 65 69 74 77)
		     (62 65 69 74 77 86) 
		     (62 69 74) 
		     (62 69 74 77 81) 
		     (62 69 74 77 81 86) 
		     (74 81)
		     (74 81 86))
	       (:MIN7 (57 62 69 72) 
		      (57 62 69 72 77) 
		      (57 62 69 72 77 81) 
		      (62 65 69 72 77)
		      (62 65 69 74 77 84) 
		      (62 69 72) 
		      (62 69 72 77 81) 
		      (62 69 74 77 81 84) 
		      (72 81)
		      (74 81 84))
	       (:MIN9 (57 65 72 76) 
		      (56 65 72 75) 
		      (64 69 72 77) 
		      (65 72 76 81) 
		      (65 76 81 84)
		      (69 76 77 84) 
		      (72 77 81 88))
	       (:MIN11 (55 62 65 72 76 81) 
		       (62 67 72 77 81 88) 
		       (64 69 72 79) 
		       (67 72 76 81)
		       (72 79 81 88) 
		       (81 72 76 79))
	       (:DIM (56 62 68 74) 
		     (56 62 68 74 77) 
		     (56 62 68 74 77 80) 
		     (62 65 68 74 77)
		     (62 65 68 74 77 86) 
		     (62 68 74) 
		     (62 68 74 77 80) 
		     (62 68 74 77 80 86) 
		     (74 80)
		     (74 80 86))
	       (:6TH (54 59 66 69 74 78) 
		     (59 66 69 74 78 83) 
		     (62 69 71 78) 
		     (62 69 74 78 83)
		     (62 71 78 81) 
		     (66 71 74 81) 
		     (69 74 78 83) 
		     (69 74 78 83 86) 
		     (71 78 81 86)
		     (74 85 83 90))
	       (:MIN6 (53 59 65 69 74 77) 
		      (59 65 69 74 77 83) 
		      (62 69 71 77) 
		      (62 69 74 77 83)
		      (62 71 77 81) 
		      (65 71 74 81) 
		      (69 74 77 83) 
		      (69 74 77 83 86) 
		      (71 77 81 86)
		      (74 85 83 89))
	       (:SUS4 (62 69 74 79 93 86) 
		      (62 69 74 91 81) 
		      (62 67 69 86) 
		      (62 69 86 79)
		      (69 74 91 86) 
		      (74 79 93 86) 
		      (74 81 98 91))
	       (:SUS2 (57 64 69 72 88) 
		      (62 69 86 76) 
		      (74 81 98 88)))
	      ((:SOLO (51) 
		      (63) 
		      (75))
	       (:MAJ (58 63 70 75) 
		     (58 63 70 75 79) 
		     (58 63 70 75 79 82) 
		     (63 67 70 75 79)
		     (63 67 70 75 79 87) 
		     (63 70 75) 
		     (63 70 75 79 82) 
		     (63 70 75 79 82 87) 
		     (75 82)
		     (75 82 87))
	       (:MAJ7 (58 63 70 74) 
		      (58 63 70 74 79) 
		      (58 63 70 74 79 82) 
		      (63 67 70 74 79)
		      (63 67 70 75 79 86) 
		      (63 70 74) 
		      (63 70 74 79 82) 
		      (63 70 75 79 82 86) 
		      (74 82)
		      (75 82 86))
	       (:DOM7 (58 63 70 73) 
		      (58 63 70 73 79) 
		      (58 63 70 73 79 82) 
		      (63 67 70 73 79)
		      (63 67 70 75 79 85) 
		      (63 70 73) 
		      (63 70 73 79 82) 
		      (63 70 75 79 82 85) 
		      (73 82)
		      (75 82 85))
	       (:MAJ9 (58 67 74 77) 
		      (57 67 74 76) 
		      (65 70 74 79) 
		      (67 74 77 82) 
		      (67 77 82 86)
		      (70 77 79 85) 
		      (74 79 82 89))
	       (:MAJ11 (56 63 67 73 77 82) 
		       (63 68 73 79 82 89) 
		       (65 70 73 80) 
		       (68 73 77 82)
		       (73 80 82 89) 
		       (82 73 77 80))
	       (:MIN (58 63 70 75) 
		     (58 63 70 75 78) 
		     (58 63 70 75 78 82) 
		     (63 66 70 75 78)
		     (63 66 70 75 78 87) 
		     (63 70 75) 
		     (63 70 75 78 82) 
		     (63 70 75 78 82 87) 
		     (75 82)
		     (75 82 87))
	       (:MIN7 (58 63 70 73) 
		      (58 63 70 73 78) 
		      (58 63 70 73 78 82) 
		      (63 66 70 73 78)
		      (63 66 70 75 78 85) 
		      (63 70 73) 
		      (63 70 73 78 82) 
		      (63 70 75 78 82 85) 
		      (73 82)
		      (75 82 85))
	       (:MIN9 (58 66 73 77) 
		      (57 66 73 76) 
		      (65 70 73 78) 
		      (66 73 77 82) 
		      (66 77 82 85)
		      (70 77 78 85) 
		      (73 78 82 89))
	       (:MIN11 (56 63 66 73 77 82) 
		       (63 68 73 78 82 89) 
		       (65 70 73 80) 
		       (68 73 77 82)
		       (73 80 82 89) 
		       (82 73 77 80))
	       (:DIM (57 63 69 75) 
		     (57 63 69 75 78) 
		     (57 63 69 75 78 81) 
		     (63 66 69 75 78)
		     (63 66 69 75 78 87) 
		     (63 69 75) 
		     (63 69 75 78 81) 
		     (63 69 75 78 81 87) 
		     (75 81)
		     (75 81 87))
	       (:6TH (55 60 67 70 75 79) 
		     (60 67 70 75 79 84) 
		     (63 70 72 79) 
		     (63 70 75 79 84)
		     (63 72 79 82) 
		     (67 72 75 82) 
		     (70 75 79 84) 
		     (70 75 79 84 87) 
		     (72 79 82 87)
		     (75 86 84 91))
	       (:MIN6 (54 60 66 70 75 78) 
		      (60 66 70 75 78 84) 
		      (63 70 72 78) 
		      (63 70 75 78 84)
		      (63 72 78 82) 
		      (66 72 75 82) 
		      (70 75 78 84) 
		      (70 75 78 84 87) 
		      (72 78 82 87)
		      (75 86 84 90))
	       (:SUS4 (63 70 75 80 82 87) 
		      (63 70 75 80 82) 
		      (63 68 70 87) 
		      (63 70 87 80)
		      (70 75 80 87) 
		      (75 80 82 87) 
		      (75 82 87 80))
	       (:SUS2 (58 65 70 73 89) 
		      (63 70 87 77) 
		      (75 82 87 89)))
	      ((:SOLO (52) 
		      (64) 
		      (76) 
		      (88))
	       (:MAJ (52 59 64) 
		     (52 59 64 71 76 80) 
		     (52 69 64 68 71 76) 
		     (56 64 68 71 76 80)
		     (56 64 71 76) 
		     (64 68 71 76 80 88) 
		     (64 71 76) 
		     (76 83) 
		     (71 64 71 76 80 83))
	       (:MAJ7 (52 59 63) 
		      (52 59 64 71 75 80) 
		      (52 69 64 68 71 75) 
		      (56 64 68 71 75 80)
		      (56 64 71 75) 
		      (64 68 71 76 80 87) 
		      (64 71 75) 
		      (75 83) 
		      (71 64 71 75 80 83))
	       (:DOM7 (52 59 62) 
		      (52 59 64 71 74 80) 
		      (52 69 64 68 71 74) 
		      (56 64 68 71 74 80)
		      (56 64 71 74) 
		      (64 68 71 76 80 86) 
		      (64 71 74) 
		      (74 83) 
		      (71 64 71 74 80 83))
	       (:MAJ9 (52 59 64 68 75 78) 
		      (54 59 64 71 75 80) 
		      (59 66 75 80) 
		      (66 71 75 80)
		      (68 75 78 83) 
		      (71 78 80 87))
	       (:MAJ11 (52 59 66 69 74) 
		       (52 66 71 74 81) 
		       (52 69 74 78 83) 
		       (54 59 64 69 74)
		       (57 66 71 74) 
		       (71 66 69 76 80 86))
	       (:MIN (52 59 64) 
		     (52 59 64 71 76 79) 
		     (52 69 64 67 71 76) 
		     (55 64 67 71 76 79)
		     (55 64 71 76) 
		     (64 67 71 76 79 88) 
		     (64 71 76) 
		     (76 83) 
		     (71 64 71 76 79 83))
	       (:MIN7 (52 59 62) 
		      (52 59 64 71 74 79) 
		      (52 69 64 67 71 74) 
		      (55 64 67 71 74 79)
		      (55 64 71 74) 
		      (64 67 71 76 79 86) 
		      (64 71 74) 
		      (74 83) 
		      (71 64 71 74 79 83))
	       (:MIN9 (52 59 64 67 74 78) 
		      (54 59 64 71 74 79) 
		      (59 66 74 79) 
		      (66 71 74 79)
		      (67 74 78 83) 
		      (71 78 79 86))
	       (:MIN11 (52 59 66 69 74) 
		       (52 66 71 74 81) 
		       (52 69 74 78 83) 
		       (54 59 64 69 74)
		       (57 66 71 74) 
		       (71 66 69 76 79 86))
	       (:DIM (52 58 64) 
		     (52 58 64 70 76 79) 
		     (52 69 64 67 70 76) 
		     (55 64 67 70 76 79)
		     (55 64 70 76) 
		     (64 67 70 76 79 88) 
		     (64 70 76) 
		     (76 82) 
		     (70 64 70 76 79 82))
	       (:6TH (56 61 68 71 76 80) 
		     (59 64 68 73 76) 
		     (64 71 73 80) 
		     (64 73 80 83)
		     (61 68 71 76 92 85) 
		     (68 73 76 83) 
		     (71 76 80 85) 
		     (71 76 80 85 88)
		     (73 80 83 88))
	       (:MIN6 (55 61 67 71 76 79) 
		      (59 64 67 73 76) 
		      (64 71 73 79) 
		      (64 73 79 83)
		      (61 67 71 76 91 85) 
		      (67 73 76 83) 
		      (71 76 79 85) 
		      (71 76 79 85 88)
		      (73 79 83 88))
	       (:SUS4 (52 59 64 69 83 76) 
		      (64 69 71 88) 
		      (64 69 83 76) 
		      (64 71 76 81 95 88)
		      (64 71 76 93 83) 
		      (64 71 88 81) 
		      (71 76 93 88) 
		      (76 81 95 88))
	       (:SUS2 (64 71 76 90 83) 
		      (64 71 88 78)))
	      ((:SOLO (53) 
		      (65) 
		      (77) 
		      (89))
	       (:MAJ (53 60 65) 
		     (53 60 65 72 77 81) 
		     (53 70 65 69 72 77) 
		     (57 65 69 72 77 81)
		     (57 65 72 77) 
		     (65 69 72 77 81 89) 
		     (65 72 77) 
		     (77 84) 
		     (72 65 72 77 81 84))
	       (:MAJ7 (53 60 64) 
		      (53 60 65 72 76 81) 
		      (53 70 65 69 72 76) 
		      (57 65 69 72 76 81)
		      (57 65 72 76) 
		      (65 69 72 77 81 88) 
		      (65 72 76) 
		      (76 84) 
		      (72 65 72 76 81 84))
	       (:DOM7 (53 60 63) 
		      (53 60 65 72 75 81) 
		      (53 70 65 69 72 75) 
		      (57 65 69 72 75 81)
		      (57 65 72 75) 
		      (65 69 72 77 81 87) 
		      (65 72 75) 
		      (75 84) 
		      (72 65 72 75 81 84))
	       (:MAJ9 (53 60 65 69 76 79) 
		      (55 60 65 72 76 81) 
		      (60 67 76 81) 
		      (67 72 76 81)
		      (69 76 79 84) 
		      (72 79 81 88))
	       (:MAJ11 (53 60 67 70 75) 
		       (53 67 72 75 82) 
		       (53 70 75 79 84) 
		       (55 60 65 70 75)
		       (58 67 72 75) 
		       (72 67 70 77 81 87))
	       (:MIN (53 60 65) 
		     (53 60 65 72 77 80) 
		     (53 70 65 68 72 77) 
		     (56 65 68 72 77 80)
		     (56 65 72 77) 
		     (65 68 72 77 80 89) 
		     (65 72 77) 
		     (77 84) 
		     (72 65 72 77 80 84))
	       (:MIN7 (53 60 63) 
		      (53 60 65 72 75 80) 
		      (53 70 65 68 72 75) 
		      (56 65 68 72 75 80)
		      (56 65 72 75) 
		      (65 68 72 77 80 87) 
		      (65 72 75) 
		      (75 84) 
		      (72 65 72 75 80 84))
	       (:MIN9 (53 60 65 68 75 79) 
		      (55 60 65 72 75 80) 
		      (60 67 75 80) 
		      (67 72 75 80)
		      (68 75 79 84) 
		      (72 79 80 87))
	       (:MIN11 (53 60 67 70 75) 
		       (53 67 72 75 82) 
		       (53 70 75 79 84) 
		       (55 60 65 70 75)
		       (58 67 72 75) 
		       (72 67 70 77 80 87))
	       (:DIM (53 59 65) 
		     (53 59 65 71 77 80) 
		     (53 70 65 68 71 77) 
		     (56 65 68 71 77 80)
		     (56 65 71 77) 
		     (65 68 71 77 80 89) 
		     (65 71 77) 
		     (77 83) 
		     (71 65 71 77 80 83))
	       (:6TH (57 62 69 72 77 81) 
		     (60 65 69 74 77) 
		     (65 72 74 81) 
		     (65 74 81 84)
		     (62 69 72 77 81 86) 
		     (69 74 77 84) 
		     (72 77 81 86) 
		     (72 77 81 86 89)
		     (74 81 84 89))
	       (:MIN6 (56 62 68 72 77 80) 
		      (60 65 68 74 77) 
		      (65 72 74 80) 
		      (65 74 80 84)
		      (62 68 72 77 80 86) 
		      (68 74 77 84) 
		      (72 77 80 86) 
		      (72 77 80 86 89)
		      (74 80 84 89))
	       (:SUS4 (53 60 65 70 84 77) 
		      (65 70 72 89) 
		      (65 70 84 77) 
		      (65 72 77 82 84 89)
		      (65 72 77 82 84) 
		      (65 72 89 82) 
		      (72 77 82 89) 
		      (77 82 84 89))
	       (:SUS2 (65 72 77 91 84) 
		      (65 72 89 79)))
	      ((:SOLO (54) 
		      (66) 
		      (78) 
		      (90))
	       (:MAJ (54 61 66) 
		     (54 61 66 73 78 82) 
		     (54 71 66 70 73 78) 
		     (58 66 70 73 78 82)
		     (58 66 73 78) 
		     (66 70 73 78 82 90) 
		     (66 73 78) 
		     (78 85) 
		     (73 66 73 78 82 85))
	       (:MAJ7 (54 61 65) 
		      (54 61 66 73 77 82) 
		      (54 71 66 70 73 77) 
		      (58 66 70 73 77 82)
		      (58 66 73 77) 
		      (66 70 73 78 82 89) 
		      (66 73 77) 
		      (77 85) 
		      (73 66 73 77 82 85))
	       (:DOM7 (54 61 64) 
		      (54 61 66 73 76 82) 
		      (54 71 66 70 73 76) 
		      (58 66 70 73 76 82)
		      (58 66 73 76) 
		      (66 70 73 78 82 88) 
		      (66 73 76) 
		      (76 85) 
		      (73 66 73 76 82 85))
	       (:MAJ9 (54 61 66 70 77 80) 
		      (56 61 66 73 77 82) 
		      (61 68 77 82) 
		      (68 73 77 82)
		      (70 77 80 85) 
		      (73 80 82 89))
	       (:MAJ11 (54 61 68 71 76) 
		       (54 68 73 76 83) 
		       (54 71 76 80 85) 
		       (56 61 66 71 76)
		       (59 68 73 76) 
		       (73 68 71 78 82 88))
	       (:MIN (54 61 66) 
		     (54 61 66 73 78 81) 
		     (54 71 66 69 73 78) 
		     (57 66 69 73 78 81)
		     (57 66 73 78) 
		     (66 69 73 78 81 90) 
		     (66 73 78) 
		     (78 85) 
		     (73 66 73 78 81 85))
	       (:MIN7 (54 61 64) 
		      (54 61 66 73 76 81) 
		      (54 71 66 69 73 76) 
		      (57 66 69 73 76 81)
		      (57 66 73 76) 
		      (66 69 73 78 81 88) 
		      (66 73 76) 
		      (76 85) 
		      (73 66 73 76 81 85))
	       (:MIN9 (54 61 66 69 76 80) 
		      (56 61 66 73 76 81) 
		      (61 68 76 81) 
		      (68 73 76 81)
		      (69 76 80 85) 
		      (73 80 81 88))
	       (:MIN11 (54 61 68 71 76) 
		       (54 68 73 76 83) 
		       (54 71 76 80 85) 
		       (56 61 66 71 76)
		       (59 68 73 76) 
		       (73 68 71 78 81 88))
	       (:DIM (54 60 66) 
		     (54 60 66 72 78 81) 
		     (54 71 66 69 72 78) 
		     (57 66 69 72 78 81)
		     (57 66 72 78) 
		     (66 69 72 78 81 90) 
		     (66 72 78) 
		     (78 84) 
		     (72 66 72 78 81 84))
	       (:6TH (58 63 70 73 78 82) 
		     (61 66 70 75 78) 
		     (66 73 75 82) 
		     (66 75 82 85)
		     (63 70 73 78 82 87) 
		     (70 75 78 85) 
		     (73 78 82 87) 
		     (73 78 82 87 90)
		     (75 82 85 90))
	       (:MIN6 (57 63 69 73 78 81) 
		      (61 66 69 75 78) 
		      (66 73 75 81) 
		      (66 75 81 85)
		      (63 69 73 78 81 87) 
		      (69 75 78 85) 
		      (73 78 81 87) 
		      (73 78 81 87 90)
		      (75 81 85 90))
	       (:SUS4 (54 61 66 71 85 78) 
		      (66 71 73 90) 
		      (66 71 85 78) 
		      (66 73 78 83 85 90)
		      (66 73 78 83 85) 
		      (66 73 90 83) 
		      (73 78 83 90) 
		      (78 83 85 90))
	       (:SUS2 (66 73 78 80 85) 
		      (66 73 90 80)))
	      ((:SOLO (55) 
		      (67) 
		      (79) 
		      (91))
	       (:MAJ (55 62 67) 
		     (55 62 67 71 74 79) 
		     (55 71 62 67 71 79) 
		     (55 71 62 67 74 79)
		     (55 74 79) 
		     (55 74 79 83 86) 
		     (59 67 71 74 79 83) 
		     (62 55 74 79 83 86)
		     (62 67 74 79) 
		     (71 74 79 86))
	       (:MAJ7 (55 62 66) 
		      (55 62 67 71 74 78) 
		      (55 71 62 67 71 78) 
		      (55 71 62 67 74 78)
		      (55 74 78) 
		      (55 74 78 83 86) 
		      (59 67 71 74 78 83) 
		      (62 55 74 78 83 86)
		      (62 67 74 78) 
		      (71 74 78 86))
	       (:DOM7 (55 62 65) 
		      (55 62 67 71 74 77) 
		      (55 71 62 67 71 77) 
		      (55 71 62 67 74 77)
		      (55 74 77) 
		      (55 74 77 83 86) 
		      (59 67 71 74 77 83) 
		      (62 55 74 77 83 86)
		      (62 67 74 77) 
		      (71 74 77 86))
	       (:MAJ9 (55 66 72 74 81) 
		      (57 62 67 62 78 83) 
		      (59 62 69 50 78) 
		      (62 67 71 78 81)
		      (62 69 72 78) 
		      (62 71 78 81))
	       (:MAJ11 (55 54 62 69 71 77) 
		       (55 60 65 71 74 81) 
		       (57 62 67 72 77 83)
		       (60 65 69 74) 
		       (60 67 71 77 81 86) 
		       (65 72 74 81))
	       (:MIN (55 62 67) 
		     (55 62 67 70 74 79) 
		     (55 70 62 67 70 79) 
		     (55 70 62 67 74 79)
		     (55 74 79) 
		     (55 74 79 82 86) 
		     (58 67 70 74 79 82) 
		     (62 55 74 79 82 86)
		     (62 67 74 79) 
		     (70 74 79 86))
	       (:MIN7 (55 62 65) 
		      (55 62 67 70 74 77) 
		      (55 70 62 67 70 77) 
		      (55 70 62 67 74 77)
		      (55 74 77) 
		      (55 74 77 82 86) 
		      (58 67 70 74 77 82) 
		      (62 55 74 77 82 86)
		      (62 67 74 77) 
		      (70 74 77 86))
	       (:MIN9 (55 65 72 74 81) 
		      (57 62 67 62 77 82) 
		      (58 62 69 50 77) 
		      (62 67 70 77 81)
		      (62 69 72 77) 
		      (62 70 77 81))
	       (:MIN11 (55 53 62 69 70 77) 
		       (55 60 65 70 74 81) 
		       (57 62 67 72 77 82)
		       (60 65 69 74) 
		       (60 67 70 77 81 86) 
		       (65 72 74 81))
	       (:DIM (55 61 67) 
		     (55 61 67 70 73 79) 
		     (55 70 61 67 70 79) 
		     (55 70 61 67 73 79)
		     (55 73 79) 
		     (55 73 79 82 85) 
		     (58 67 70 73 79 82) 
		     (61 55 73 79 82 85)
		     (61 67 73 79) 
		     (70 73 79 85))
	       (:6TH (52 59 62 67 71 76) 
		     (55 64 71 74) 
		     (59 64 71 74 79 83) 
		     (62 67 71 76)
		     (64 71 74 79) 
		     (64 71 74 79 83 88) 
		     (67 74 76 83) 
		     (67 74 79 83 88)
		     (71 76 79 86) 
		     (74 79 83 88))
	       (:MIN6 (52 58 62 67 70 76) 
		      (55 64 70 74) 
		      (58 64 70 74 79 82) 
		      (62 67 70 76)
		      (64 70 74 79) 
		      (64 70 74 79 82 88) 
		      (67 74 76 82) 
		      (67 74 79 82 88)
		     (70 76 79 86) 
		     (74 79 82 88))
	       (:SUS4 (55 62 67 72 86 79) 
		      (62 67 84 79) 
		      (67 72 74 91) 
		      (67 72 86 79)
		      (67 74 79 96 86) 
		      (67 74 91 84))
	       (:SUS2 (67 74 79 93 86) 
		      (67 74 91 81)))
	      ((:SOLO (56) 
		      (68) 
		      (80) 
		      (80))
	       (:MAJ (56 63 68) 
		     (56 63 68 72 75 80) 
		     (56 72 63 68 72 80) 
		     (56 72 63 68 75 80)
		     (56 75 80) 
		     (56 75 80 84 87) 
		     (60 68 72 75 80 84) 
		     (63 56 75 80 84 87)
		     (63 68 75 80) 
		     (72 75 80 87))
	       (:MAJ7 (56 63 67) 
		      (56 63 68 72 75 79) 
		      (56 72 63 68 72 79) 
		      (56 72 63 68 75 79)
		      (56 75 79) 
		      (56 75 79 84 87) 
		      (60 68 72 75 79 84) 
		      (63 56 75 79 84 87)
		      (63 68 75 79) 
		      (72 75 79 87))
	       (:DOM7 (56 63 66) 
		      (56 63 68 72 75 78) 
		      (56 72 63 68 72 78) 
		      (56 72 63 68 75 78)
		      (56 75 78) 
		      (56 75 78 84 87) 
		      (60 68 72 75 78 84) 
		      (63 56 75 78 84 87)
		      (63 68 75 78) 
		      (72 75 78 87))
	       (:MAJ9 (56 67 73 75 82) 
		      (58 63 68 63 79 84) 
		      (60 63 70 51 79) 
		      (63 68 72 79 82)
		      (63 70 73 79) 
		      (63 72 79 82))
	       (:MAJ11 (56 55 63 70 72 78) 
		       (56 61 66 72 75 82) 
		       (58 63 68 73 78 84)
		       (61 66 70 75) 
		       (61 68 72 78 82 87) 
		       (66 73 75 82))
	       (:MIN (56 63 68) 
		     (56 63 68 71 75 80) 
		     (56 71 63 68 71 80) 
		     (56 71 63 68 75 80)
		     (56 75 80) 
		     (56 75 80 83 87) 
		     (59 68 71 75 80 83) 
		     (63 56 75 80 83 87)
		     (63 68 75 80) 
		     (71 75 80 87))
	       (:MIN7 (56 63 66) 
		      (56 63 68 71 75 78) 
		      (56 71 63 68 71 78) 
		      (56 71 63 68 75 78)
		      (56 75 78) 
		      (56 75 78 83 87) 
		      (59 68 71 75 78 83) 
		      (63 56 75 78 83 87)
		      (63 68 75 78) 
		      (71 75 78 87))
	       (:MIN9 (56 66 73 75 82) 
		      (58 63 68 63 78 83) 
		      (59 63 70 51 78) 
		      (63 68 71 78 82)
		      (63 70 73 78) 
		      (63 71 78 82))
	       (:MIN11 (56 54 63 70 71 78) 
		       (56 61 66 71 75 82) 
		       (58 63 68 73 78 83)
		       (61 66 70 75) 
		       (61 68 71 78 82 87) 
		       (66 73 75 82))
	       (:DIM (56 62 68) 
		     (56 62 68 71 74 80) 
		     (56 71 62 68 71 80) 
		     (56 71 62 68 74 80)
		     (56 74 80) 
		     (56 74 80 83 86) 
		     (59 68 71 74 80 83) 
		     (62 56 74 80 83 86)
		     (62 68 74 80) 
		     (71 74 80 86))
	       (:6TH (53 60 63 68 72 77) 
		     (56 65 72 75) 
		     (60 65 72 75 80 84) 
		     (63 68 72 77)
		     (65 72 75 80) 
		     (65 72 75 80 84 89) 
		     (68 75 77 84) 
		     (68 75 80 84 89)
		     (72 77 80 87) 
		     (75 80 84 89))
	       (:MIN6 (53 59 63 68 71 77) 
		      (56 65 71 75) 
		      (59 65 71 75 80 83) 
		      (63 68 71 77)
		      (65 71 75 80) 
		      (65 71 75 80 83 89) 
		      (68 75 77 83) 
		      (68 75 80 83 89)
		      (71 77 80 87) 
		      (75 80 83 89))
	       (:SUS4 (56 63 68 73 87 80) 
		      (63 68 85 80) 
		      (68 73 75 80) 
		      (68 73 87 80)
		      (68 75 80 85 87) 
		      (68 75 80 85))
	       (:SUS2 (68 75 80 82 87) 
		      (68 75 80 82)))
	      ((:SOLO (57) 
		      (69) 
		      (81))
	       (:MAJ (52 57 64 69 73 76) 
		     (57 61 64 69 73) 
		     (57 64 69) 
		     (57 64 69 73 76 81)
		     (57 69 76 81) 
		     (57 73 76 69 88) 
		     (61 69 73 76 69 85) 
		     (64 69 76 69 73))
	       (:MAJ7 (52 57 64 68 73 76) 
		      (57 61 64 68 73) 
		      (57 64 68) 
		      (57 64 69 73 76 80)
		      (57 69 76 80) 
		      (57 73 76 68 88) 
		      (61 68 73 76 68 85) 
		      (64 68 76 68 73))
	       (:DOM7 (52 57 64 67 73 76) 
		      (57 61 64 67 73) 
		      (57 64 67) 
		      (57 64 69 73 76 79)
		      (57 69 76 79) 
		      (57 73 76 67 88) 
		      (61 67 73 76 67 85) 
		      (64 67 76 67 73))
	       (:MAJ9 (57 64 71 73 80) 
		      (57 73 76 80 83) 
		      (59 64 69 73 80) 
		      (59 64 69 76 80 85)
		      (64 69 73 80 83) 
		      (64 71 73 80))
	       (:MAJ11 (52 59 62 69 73 79) 
		       (57 62 67 73 76 83) 
		       (57 64 67 74 76 83)
		       (57 64 71 74 79) 
		       (61 69 74 79 83 88) 
		       (71 64 69 74 79 85))
	       (:MIN (52 57 64 69 72 76) 
		     (57 60 64 69 72) 
		     (57 64 69) 
		     (57 64 69 72 76 81)
		     (57 69 76 81) 
		     (57 72 76 69 88) 
		     (60 69 72 76 69 84) 
		     (64 69 76 69 72))
	       (:MIN7 (52 57 64 67 72 76) 
		      (57 60 64 67 72) 
		      (57 64 67) 
		      (57 64 69 72 76 79)
		      (57 69 76 79) 
		      (57 72 76 67 88) 
		      (60 67 72 76 67 84) 
		      (64 67 76 67 72))
	       (:MIN9 (57 64 71 72 79) 
		      (57 72 76 79 83) 
		      (59 64 69 72 79) 
		      (59 64 69 76 79 84)
		      (64 69 72 79 83) 
		      (64 71 72 79))
	       (:MIN11 (52 59 62 69 72 79) 
		       (57 62 67 72 76 83) 
		       (57 64 67 74 76 83)
		       (57 64 71 74 79) 
		       (60 69 74 79 83 88) 
		       (71 64 69 74 79 84))
	       (:DIM (51 57 63 69 72 75) 
		     (57 60 63 69 72) 
		     (57 63 69) 
		     (57 63 69 72 75 81)
		     (57 69 75 81) 
		     (57 72 75 69 87) 
		     (60 69 72 75 69 84) 
		     (63 69 75 69 72))
	       (:6TH (57 66 73 76) 
		     (57 64 69 73 78) 
		     (54 61 64 69 73 78) 
		     (61 66 73 76 81 85)
		     (64 69 73 78) 
		     (66 73 76 81) 
		     (69 76 78 85) 
		     (69 76 81 85 90) 
		     (73 78 81 88)
		     (76 81 85 90))
	       (:MIN6 (57 66 72 76) 
		      (57 64 69 72 78) 
		      (54 60 64 69 72 78) 
		      (60 66 72 76 81 84)
		      (64 69 72 78) 
		      (66 72 76 81) 
		      (69 76 78 84) 
		      (69 76 81 84 90) 
		      (72 78 81 88)
		      (76 81 84 90))
	       (:SUS4 (57 64 69 86 76) 
		      (57 64 69 74 88 81) 
		      (64 69 86 81) 
		      (69 74 76 93)
		      (69 74 88 81) 
		      (69 76 81 98 88) 
		      (69 76 93 86))
	       (:SUS2 (57 64 69 83 76) 
		      (69 76 81 95 88) 
		      (69 76 93 83)))
	      ((:SOLO (58) 
		      (70) 
		      (82))
	       (:MAJ (53 58 65 70 74 77) 
		     (58 62 65 70 74) 
		     (58 65 70) 
		     (58 65 70 74 77 82)
		     (58 70 77 82) 
		     (58 74 77 70 89) 
		     (62 70 74 77 70 86) 
		     (65 70 77 70 74))
	       (:MAJ7 (53 58 65 69 74 77) 
		      (58 62 65 69 74) 
		      (58 65 69) 
		      (58 65 70 74 77 81)
		      (58 70 77 81) 
		      (58 74 77 69 89) 
		      (62 69 74 77 69 86) 
		      (65 69 77 69 74))
	       (:DOM7 (53 58 65 68 74 77) 
		      (58 62 65 68 74) 
		      (58 65 68) 
		      (58 65 70 74 77 80)
		      (58 70 77 80) 
		      (58 74 77 68 89) 
		      (62 68 74 77 68 86) 
		      (65 68 77 68 74))
	       (:MAJ9 (58 65 72 74 81) 
		      (58 74 77 81 84) 
		      (60 65 70 74 81) 
		      (60 65 70 77 81 86)
		      (65 70 74 81 84) 
		      (65 72 74 81))
	       (:MAJ11 (53 60 63 70 74 80) 
		       (58 63 68 74 77 84) 
		       (58 65 68 75 77 84)
		       (58 65 72 75 80) 
		       (62 70 75 80 84 89) 
		       (72 65 70 75 80 86))
	       (:MIN (53 58 65 70 73 77) 
		     (58 61 65 70 73) 
		     (58 65 70) 
		     (58 65 70 73 77 82)
		     (58 70 77 82) 
		     (58 73 77 70 89) 
		     (61 70 73 77 70 85) 
		     (65 70 77 70 73))
	       (:MIN7 (53 58 65 68 73 77) 
		      (58 61 65 68 73) 
		      (58 65 68) 
		      (58 65 70 73 77 80)
		      (58 70 77 80) 
		      (58 73 77 68 89) 
		      (61 68 73 77 68 85) 
		      (65 68 77 68 73))
	       (:MIN9 (58 65 72 73 80) 
		      (58 73 77 80 84) 
		      (60 65 70 73 80) 
		      (60 65 70 77 80 85)
		      (65 70 73 80 84) 
		      (65 72 73 80))
	       (:MIN11 (53 60 63 70 73 80) 
		       (58 63 68 73 77 84) 
		       (58 65 68 75 77 84)
		       (58 65 72 75 80) 
		       (61 70 75 80 84 89) 
		       (72 65 70 75 80 85))
	       (:DIM (52 58 64 70 73 76) 
		     (58 61 64 70 73) 
		     (58 64 70) 
		     (58 64 70 73 76 82)
		     (58 70 76 82) 
		     (58 73 76 70 88) 
		     (61 70 73 76 70 85) 
		     (64 70 76 70 73))
	       (:6TH (58 67 74 77) 
		     (58 65 70 74 79) 
		     (55 62 65 70 74 79) 
		     (62 67 74 77 82 86)
		     (65 70 74 79) 
		     (67 74 77 82) 
		     (70 77 79 86) 
		     (70 77 82 86 91) 
		     (74 79 82 89)
		     (77 82 86 91))
	       (:MIN6 (58 67 73 77) 
		      (58 65 70 73 79) 
		      (55 61 65 70 73 79) 
		      (61 67 73 77 82 85)
		      (65 70 73 79) 
		      (67 73 77 82) 
		      (70 77 79 85) 
		      (70 77 82 85 91) 
		      (73 79 82 89)
		      (77 82 85 91))
	       (:SUS4 (58 65 70 87 77) 
		      (58 65 70 75 89 82) 
		      (65 70 87 82) 
		      (70 75 77 82)
		      (70 75 89 82) 
		      (70 77 82 87 89) 
		      (70 77 82 87))
	       (:SUS2 (58 65 70 84 77) 
		      (70 77 82 84 89) 
		      (70 77 82 84)))
	      ((:SOLO (59) 
		      (71) 
		      (83))
	       (:MAJ (54 59 66 71 63 78) 
		     (59 63 66 71 63) 
		     (59 66 71) 
		     (59 66 71 75 78 83)
		     (63 71 78 83) 
		     (66 71 78 83 87) 
		     (71 78 83))
	       (:MAJ7 (54 59 66 70 63 78) 
		      (59 63 66 70 63) 
		      (59 66 70) 
		      (59 66 71 75 78 82)
		      (63 71 78 82) 
		      (66 71 78 82 87) 
		      (71 78 82))
	       (:DOM7 (54 59 66 69 63 78) 
		      (59 63 66 69 63) 
		      (59 66 69) 
		      (59 66 71 75 78 81)
		      (63 71 78 81) 
		      (66 71 78 81 87) 
		      (71 78 81))
	       (:MAJ9 (54 59 63 66 73) 
		      (61 66 70 75) 
		      (61 66 71 75 82) 
		      (66 71 75 82 85)
		      (71 66 73 66 82 87) 
		      (82 75 78 73))
	       (:MAJ11 (54 59 64 69 73 78) 
		       (69 76 78 87) 
		       (71 64 69 75 78 85)
		       (61 66 71 76 81 87) 
		       (66 85 76 81) 
		       (73 78 81 88))
	       (:MIN (54 59 66 71 62 78) 
		     (59 62 66 71 62) 
		     (59 66 71) 
		     (59 66 71 74 78 83)
		     (62 71 78 83) 
		     (66 71 78 83 86) 
		     (71 78 83))
	       (:MIN7 (54 59 66 69 62 78) 
		      (59 62 66 69 62) 
		      (59 66 69) 
		      (59 66 71 74 78 81)
		      (62 71 78 81) 
		      (66 71 78 81 86) 
		      (71 78 81))
	       (:MIN9 (54 59 62 66 73) 
		      (61 66 69 74) 
		      (61 66 71 74 81) 
		      (66 71 74 81 85)
		      (71 66 73 66 81 86) 
		      (81 74 78 73))
	       (:MIN11 (54 59 64 69 73 78) 
		       (69 76 78 86) 
		       (71 64 69 74 78 85)
		       (61 66 71 76 81 86) 
		       (66 85 76 81) 
		       (73 78 81 88))
	       (:DIM (53 59 65 71 62 77) 
		     (59 62 65 71 62) 
		     (59 65 71) 
		     (59 65 71 74 77 83)
		     (62 71 77 83) 
		     (65 71 77 83 86) 
		     (71 77 83))
	       (:6TH (56 63 66 71 75 80) 
		     (59 68 75 78) 
		     (63 68 75 78 83 87) 
		     (63 66 71 78)
		     (66 71 75 80 83) 
		     (68 75 78 83) 
		     (71 78 80 87) 
		     (75 80 83 90))
	       (:MIN6 (56 62 66 71 74 80) 
		      (59 68 74 78) 
		      (62 68 74 78 83 86) 
		      (62 66 71 78)
		      (66 71 74 80 83) 
		      (68 74 78 83) 
		      (71 78 80 86) 
		      (74 80 83 90))
	       (:SUS4 (59 66 71 76 90 83) 
		      (59 66 71 88 78) 
		      (71 66 83 88) 
		      (71 76 78 95)
		      (71 76 90 83) 
		      (71 78 95 88))
	       (:SUS2 (59 66 71 85 78) 
		      (71 78 95 85)))))


(defun gtrchord-family (pc family)
  "Returns family of guitar chords.
   ARGS:
     pc     - pitch class
     family - keyword, one of +GTR-CHORD-FAMILIES+
   RETURNS list (NIL if family is invalid)"
    (cdr (assoc family (aref +GTR-CHORDS+ (pitch-class pc)))))

(defun gtrchord (pc &optional (family :maj)(position 0))
  "Returns selected guitar chord.
   ARGS:
     pc       - Pitch class
     family   - Keyword, one of +GTR-CHORD-FAMILIES+
     position - fixnum, chord variation.  In general higher values
                are located higher on the neck.
    RETURNS list of keynumbers.
       If family or position are invalid, displays warning and returns
       single note chord."
  (or
   (nth position (cdr (gtrchord-family pc family)))
   (progn
     (cyco-warning (format nil "GTRCHORD ~a ~a ~a does not exists."
			   pc family position))
     (list pc))))

(defun gtrchord-variations (pc family)
  "Returns number of variations for selected chord.
   ARGS:
     pc     - Pitch class
     family - Keyword, one of +GTR-CHORD-FAMILIES+
   RETURNS: fixnum, negative value indicates an invalid chord family."
  (1- (length (assoc family (aref +GTR-CHORDS+ (pitch-class pc))))))

(defun ?gtrchords ()
  "Displays table of guitar chord variations."
  (let ((header "       "))
    (dotimes (pc 12)
      (setf header (str+ header (format nil "~3A " (pitch-class-name pc)))))
    (format t "~A~%" header)
    (dolist (fam-name +GTR-CHORD-FAMILIES+)
      (let* ((acc (format nil "~5A  " fam-name)))
	(dotimes (pc 12)
	  (let ((members (gtrchord-family pc fam-name)))
	    (setf acc (str+ acc (format nil "~3A " (1- (length members)))))))
	(format t "~A~%" acc)))
    (format t "Number of variations for each pitch-class/chord-type.~%")))
