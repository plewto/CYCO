;;;; CYCO local Emu
;;;;

(in-package :cyco)

(param emu (create-instrument 'EMU
			      :transient nil))

(load-local "emu/procussion/procussion")
