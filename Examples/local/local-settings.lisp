

(in-package :cyco)

;;; Primary MIDI channel assignments
;;;
(assign-channel! :TXA     1)		; Yamaha TX816
(assign-channel! :TXB     2)
(assign-channel! :TXC     3)
(assign-channel! :SY35    4)		; Yamaha SY35
(assign-channel! :R3      5)		; Korg R3
(assign-channel! :QL1     6)		; Quantum Leap sample library
(assign-channel! :QL2     7)
(assign-channel! :QL3     8)
(assign-channel! :QL4     9)
(assign-channel! :QL5    10)
(assign-channel! :PRO1   11)		; Emu Procussion
(assign-channel! :PRO2   12)
(assign-channel! :PRO3   13)
(assign-channel! :LLIA   14)
(assign-channel! :MU100R 15)		; Yamaha MU100R
(assign-channel! :BEEP   16)		; Metronome -> Procussion

;;; Secondary MIDI channel assignments
;;;
(assign-channel! :SAMPLER    1)		; Korg micro sampler
(assign-channel! :OBX        2)		; Oberheim Matrix 1000
(assign-channel! :VOP2       3)		; Alternate Voices of Passion
(assign-channel! :MU100R2    4)		; Alternate  MUR100R
(assign-channel! :LLIA2     15)		; Alternate Llia
(assign-channel! :BASS      :QL1)	; Ministry of Rock II Bass
(assign-channel! :GUITAR    :QL2)	; Ministry of Rock II / Gypsy Guitar 
(assign-channel! :GYPSY1    :QL3)	; Gypsy
(assign-channel! :VOP1      :QL4)       ; Voices of Passion
(assign-channel! :DRUMS     :QL5)	; Ministry of Rock II Drums
(assign-channel! :PRO-KIT   :PRO1)	; Procussion standard kit
(assign-channel! :PRO-CYM   :PRO2)	; Procussion cymbals
(assign-channel! :PRO-SHAKE :PRO3)	; Procussion shakers

(free-orchestra!)
(load-local "yamaha/yamaha")
(load-local "emu/emu")
;; (load-local "quantumleap/quantumleap")
(load-local "korg/korg")
(load-local "oberheim/oberheim")
(load-local "llia/llia")


(setf *metronome-instrument* (metronome-instrument
			      :channel :beep
			      :beep   '(0 ff)
			      :accent '(1 fff)
			      :phrase '(2 fff)))
