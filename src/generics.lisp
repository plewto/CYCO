;;;; cyco.generics
;;;;

(in-package :cyco)


;;; ---------------------------------------------------------------------- 
;;;				predicates

(defmacro defpredicate (name &key (docstring "Predicate"))
  "Creates generic predicate function name and a single
   implementation (name ((obj t))) which is always false.
   Methods which return true should be manually created."
  `(progn
     (defgeneric ,name (obj)
       (:documentation ,docstring))
     (defmethod ,name ((obj t)) nil)))
     
(defpredicate amplitude-p)
(defpredicate bend-event-p)
(defpredicate channel-event-p)
(defpredicate control-event-p)
(defpredicate pressure-event-p)
(defpredicate end-of-track-p)
(defpredicate instrument-p)
(defpredicate key-event-p)
(defpredicate keynumber-p)
(defpredicate meta-event-p)
(defpredicate metric-p)
(defpredicate midi-event-p)
(defpredicate nameable-p)
(defpredicate node-p)
(defpredicate part-p)
(defpredicate pattern-p)
(defpredicate program-event-p)
(defpredicate pressure-event-p)
(defpredicate project-p)
;(defpredicate rest-p)
(defpredicate root-p)
(defpredicate section-p)
(defpredicate system-common-event-p)
(defpredicate system-exclusive-event-p)
(defpredicate tempo-event-p)
(defpredicate text-event-p)
(defpredicate timesig-event-p)
(defpredicate transposable-p)



(defgeneric ->midi (obj &key filename offset repeat pad-end)
  (:documentation
   "Render events in object and write to MIDI file.
    obj - The object to render.
    :filename - Output filename.  By default the file name is the
                name of the object with .mid append, and is saved 
                in the MIDI folder under the current project folder.
                See *default-project-midi-directory*
    :offset  - Time in seconds added to each MIDI event, default 0
    :repeat  - Number of times to repeat MIDI data.  Some implementations
               may not support repeat. Default 1.
    :pad-end - Number of seconds to extend MIDI track past final even.t
               pad-end is used to prevent cutting off final release tails.
               Default 2.0 seconds."))

(defgeneric ->pattern (obj)
  (:documentation
   "Returns pattern containing object.
    If obj is a pattern, it is returned as is.
    If obj is a list, the result is (cycle :of ibj)
    Otherwise the result is (cycle :of (->list obj))"))

(defgeneric ->string (obj)
  (:documentation
   "Return string representation of an object.  If the obj is a string
    simply return it."))

(defmethod ->string ((obj t))
  (format nil "~A" obj))

(defgeneric ->symbol (obj)
  (:documentation
   "Creates symbol with same name as object (converted to uppercase).
    If obj is a symbol, simply return it."))

(defmethod ->symbol ((s symbol)) s)
(defmethod ->symbol ((s string))
  (make-symbol (string-upcase s)))

(defgeneric ->list (obj)
  (:documentation
   "Return list which contains obj.
    If obj is already a list, simply return it.
    If obj is a vector, convert to list and return."))

(defgeneric ->vector (obj)
  (:documentation
   "Return vector which contains obj.
    If obj is already a vector, simply return it.
    If obj is a list, convert to vector.
    Otherwise return a new vector holding obj."))

(defgeneric name (obj)
  (:documentation
   "Return objects name.  If name is not defined for an object the
    default is to return the string representation of the object.
    See ->string."))
   
(defgeneric name! (obj new-name)
  (:documentation
   "sets an objects name."))

(defmethod name ((obj t)) (->string obj))


;; Add child node to parent.
;;
(defgeneric add-child! (parent child &key test)
  (:documentation
   "Add child to parent if test function returns true.
    The test function takes two arguments (lambda (nd ch)...) and returns
    boolean.  Arguments nd and ch are the parent and child objects
    respectively.

    Returns child if it was added to parent, otherwise returns nil."))
  
(defmethod add-child! ((parent null)(child t) &key (test nil))
  "Specialized method, (does not) add child to nil.
   child and test arguments are ignored.  Returns nil"
  (dismiss parent child test)
  nil)

(defgeneric amplitude (opj &key range)
  (:documentation
   "Returns amplitude of obj.
    Amplitudes are normalized between 0 and 1 inclusive.
    :range - cons (min . max)  limits possible output values to between
    min and max.  Default (0 . 1)"))

(defgeneric amplitude-map (obj)
  (:documentation
   "Returns amplitude mapping function used by object."))
   
(defgeneric amplitude-map! (obj fn)
  (:documentation
   "Set amplitude mapping function for object.
    fn - Functions of one value (lambda (n)) -> n'"))
    
(defgeneric amplitude-symbol (obj)
  (:documentation
   "Returns symbolic amplitude equivalent of obj."))
  
(defgeneric amplitude->velocity (obj &key range)
  (:documentation
   "Converts amplitude in range (0..1) to MIDI velocity with 
    range (0..127).

    :range - cons (min . max) limit output values to be between min 
             and max. The range values are applies to the amplitude,
             not the velocity.  Default (0..1)."))

(defgeneric bar-duration (obj)
  (:documentation
   "Returns duration in seconds of a single bar of obj.
    See timesig."))

(defgeneric beat-duration (obj)
  (:documentation
   "Returns duration in seconds of a single beat of object.
    See timesig."))

(defgeneric butfinal (obj)
  (:documentation
   "Returns new object as the same type as obj, but with the final 
    element removed."))

(defgeneric cardinality (obj)
  (:documentation
   "Returns the number of elements in object."))

(defmethod cardinality ((obj number))
  "The cardinality of a number is that number."
  obj)

(defmethod cardinality ((seq sequence))
  "The cardinality of a sequence is the length of the sequence."
  (length seq))
  
(defgeneric channel (obj &key resolve)
  (:documentation
   "Returns the MIDI channel of an object.  
    MIDI channels may be symbolic. If :resolve is true convert 
    symbolic channel to actual MIDI channel (1..16)."))

(defgeneric channel! (obj chan)
  (:documentation
   "Set MIDI channel of object."))
  
(defgeneric channel-index (obj)
  (:documentation
   "Return channel-index of object.
    MIDI channels are defined between 1 and 16 inclusive. For actual 
    transmission they are between 0 and 15.  The term 'channel-index' 
    is used to describe the actual byte value of a MIDI channel."))

(defmethod channel-index ((obj t))
  (1- (channel obj :resolve t)))

(defgeneric channel-index! (obj n))

(defgeneric clone (obj &key newname parent hook)
  (:documentation
   "Returns cloned copy of object with possible modifications.
    For types where specialized clone method is not defined, return the 
    source object.  See DEFCLONE.

    obj - Object to be cloned.
    :newname - Optional format string for cloned object name.
               Ignore if obj type is not nameable.
               Default '~A-clone'  ~A Replaced by source object name.
    :parent  - Optional new parent.
               Ignore if obj type does not support parents.
               Defaults to parent of obj.
    :hook    - Optional function (hook n) -> n' applied to cloned
               copy.  Default #'identity"))

(defmethod clone ((obj t) &key newname parent (hook #'identity))
  (dismiss newname parent)
  (funcall hook obj))

(defgeneric dump (obj &key depth max-depth)
  (:documentation
   "Produce diagnostic dump of object.
   If obj has child objects, these too are dumped recursively,
   up to max-depth.  
   depth - int, sets current recursive depth (and indentation), default 0.
   max-depth - the maximum levels to recursively dump, default 10."))

(defmethod dump ((obj t) &key (depth 0)(max-depth :ignore))
  (if (< depth max-depth)
      (format t "~A~A ~A~%" (tab depth)(type-of obj)(name obj)))
  nil)

(defgeneric duration (obj)
  (:documentation
   "Returns float, the duration of obj in seconds."))

(defgeneric duration-map (obj)
  (:documentation
   "Returns duration mapping function of object."))

(defgeneric duration-map! (obj fn)
  (:documentation
   "Sets duration mapping function of object.
    The function fn has the form (lambda (n)) -> n'"))

(defgeneric final (obj)
  (:documentation
   "Returns final element of sequence."))

(defgeneric key-map (obj))
(defgeneric key-map! (obj km))

(defgeneric keynumber (obj)
  (:documentation
   "Convert object (or list of objects) to MIDI key number(s)."))

(defgeneric keyname (obj)
  (:documentation
   "Return name MIDI key name for object.
    (keyname 60)  -> 'c5)
    (keyname 'c5) -> 'c5)
    (keyname '(60 61 62)) -> (C5 CS5 D5)"))

(defgeneric metric (obj)
  (:documentation
   "Returns metric values of object, or list of objects"))

(defgeneric mute (obj flag))

(defgeneric next-1 (obj)
  (:documentation
   "Returns the next values from a pattern."))

(defgeneric next-n (obj n)
  (:documentation
   "Returns the next n values of a pattern as a list."))
  
(defgeneric next (obj &optional n)
  (:documentation
   "Returns the next values of a pattern.
    Optional n argument defaults to calling next-1
    n is :all, return list of all pattern values.
    n is :rest, return list of all remaining values.
    n is integer, return list of n values."))

(defmethod next-1 ((obj t)) obj)

(defmethod next-n ((obj t)(n fixnum))
  (let ((acc '()))
    (dotimes (i n)
      (push (next-1 obj) acc))
    (reverse acc)))

;; (defgeneric note-off (obj time key-number amplitude))
;; (defgeneric note-on (obj time key-number amplitude))

(defgeneric octave (obj)
  (:documentation
   "Return octave number of object, or list of octave numbers.
    (octave 60) -> 5
    (octave 'C5) -> 5
    (octave '(C4 C5 C6)) -> (4 5 6)."))

(defgeneric orchestra (&key project))

(defgeneric period (obj))

(defgeneric pick (obj &key n random-state)
  (:documentation
   "Pick n random objects from obj.
    :n - default 1.
    For n = 1, return selected object.
    For n > 1, return list."))

(defgeneric pitch-class (obj)
  (:documentation
   "Returns pitch-class of keynumber or list of keynumbers.
    (pitch-class 60)  -> 0
    (pitch-class 'g2) -> 7
    (pitch-class '(10 11 12 13)) -> (10 11 0 1)."))
  
(defgeneric phrase-duration (obj)
  (:documentation
   "Returns phrase duration of obj in seconds.
    See timesig."))
  
;; (defgeneric poly-pressure (obj time key-number value))

(defgeneric property (obj key &key default)
  (:documentation
   "Returns object property.
    key - symbol 
    default - result if key is not defined.
              The default default is :ERROR"))
  
(defgeneric property! (obj key value)
  (:documentation
   "Set property value of object.
    key - symbol
    value - property value."))

(defgeneric property-keys (obj &optional local-only)
  (:documentation
   "Returns a list of all property keys.
    :local-only - The default behavior is to return all unique keys defined
                  by obj and all ancestors of obj.  If local-only is true,
                  do not included ancestor properties."))

(defgeneric push-event (time evn obj))

(defgeneric remaining (obj)
  (:documentation
   "Returns all remaining values of pattern.
    See next."))

(defgeneric remarks (obj)
  (:documentation
   "Returns remark text of object."))

(defgeneric remarks! (obj txt)
  (:documentation
   "Sets object remarks text."))

(defgeneric retrograde! (obj))

(defgeneric render-once (obj &key offset)
  (:documentation
   "Render an object to MIDI events one time.
    :offset - time shift, default 0
    Returns list of events of form
        ((time1 . event1)(time2 . event2)...(timen . eventn))
    The list is not guaranteed to be time sorted."))
  
(defgeneric render (obj &key repeat offset)
  (:documentation
   "Render an object to MIDI events.
    :repeat - Number of times to repeat events, default 1.
              Each subsequent repeat is time shifted by the 
              duration/period of the object.
   :offset - Initial time offset in seconds, default 0.
   Returns list of form 
               ((time1 . event1)(time2 . event2)...(timen . eventn))
    The list is not guaranteed to be time sorted."))
  
(defgeneric render-event (obj))

(defgeneric rest-p (obj))

(defgeneric reset (obj))
(defmethod reset ((obj t)) nil)

(defgeneric retrograde (onj))

(defgeneric solo (obj))

;;(defgeneric subbeats (obj))
(defgeneric subbeat-duration (obj)
  (:documentation
   "Return subbeat-duration of object in seconds."))

(defgeneric tick-duration (obj &optional unit))
  
(defgeneric tie+ (args)
  (:documentation
   "Tie list of metric values together, return time in seconds."))
  
(defgeneric tie- (args))
  
(defgeneric tempo (obj)
  (:documentation
   "Returns tempo of object in BPM."))

(defgeneric tempo! (obj n)
  (:documentation
   "Sets object tempo to n BPM."))
  
(defgeneric tsubbeat-duration (obj)
  (:documentation
   "Returns duration of objects tsubbeat in seconds."))

(defgeneric text (obj))
(defgeneric text! (obj new-text))



(defgeneric filter (obj &key test)
  (:documentation
   "Return version of obj with only those elements for which test is true.
    TODO: This is poorly worded!
    test (lambda (x)) --> bool"))

(defgeneric filter! (obj &key test)
  (:documentation
   "Remove all elements of obj for which test is false
    test (lambda (x)) --> bool"))


;;; ---------------------------------------------------------------------- 
;;;			      Transformations

(defgeneric invert (a pivot &key range))

(defgeneric invert! (a pivot &key range))

(defgeneric transposable (obj flag)) ;; ALSO applies to inversion

(defgeneric transpose (a x &key range))

(defgeneric transpose! (a x &key range))

