;;;; cyco.globals
;;;;
;;;; Defines global cyco variables.
;;;;
(in-package :cyco)

(global *cyco-config-directory* ""
	"Path to cyco configuration directory.")

(global *cyco-config-file* ""
	"Name of configuration file within *cyco-config-directory*")

(global *cyco-projects-directory* ""
	"Path where cyco projects may be found.")
	
(global *default-project-boot-file* "main"
	"Name of the main file in a cyco project.")

(global *default-project-midi-directory* "MIDI"
	"Name of directory in cyco project where MIDI files are saved.")

(global *global-channel-assignments* nil
	"Global MIDI channel mappings. 
         See cyco/orch/channel.lisp")

(global *global-controller-assignments* nil
	"Global MIDI controller name assignments.
         See cyco/orch/controller.lisp")

(global *global-chord-dictionary* nil
	"Default named chord dictionary.
         See cyco/comp/chords")

(global *max-channel-assignment-nest-depth* 6
	"Maximum depth of symbolic MIDI channel assignments.
         See cyco/orch/channel.lisp")

(global *metronome-instrument* nil)

(global *os-extension-separator* "."
	"Character (as string) used to separate filename from extension.
         See cyco/util/os.lisp")

(global *os-path-separator* #\/
	"Character used to separate path components.
         See cyco/util/os.lisp")

(global *os-root* #\/
	"Character used to indicate root directory.
         See cyco/util/os.lisp")

(global *project* nil
	"The current project.
         See cyco/comp/project.lisp")
	
(global *root-instrument* nil
	"The root node for the orchestra tree.
         See cyco/orch/instrument.lisp")

(global *root-timesig* nil
	"The master default time signature object.
         See cyco/comp/project.lisp
             cyco/comp/timesig.lisp")
