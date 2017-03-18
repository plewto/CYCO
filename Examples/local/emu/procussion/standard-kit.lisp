;;;; cyco local emu procussion standard-ket
;;;;
;;;; Instruments
;;;;   Standard-kit
;;;;       pkick
;;;;       psnare
;;;;       ptom
;;;;       phat
;;;;       pcym
;;;;       ptuned
;;;;
;;;; Programs
;;;;    ampitheater
;;;;    mega-drums
;;;;    rock-n-roll
;;;;    palladium
;;;;    jazz-drums
;;;;    metal-drums
;;;;    rap-session
;;;;    ambient-rock
;;;;    acoustic-kit
;;;;    rock-drums
;;;;    house-machine
;;;;    fusion-stix
;;;;    space-drums
;;;;    hard-rock
;;;;    stadium-rox
;;;;    dance-2000
;;;;    heavy-metal
;;;;    hip-hop
;;;;    sluggo-drums
;;;;    huge-room
;;;;    drum-dance
;;;;    heavyosity
;;;;    dance-club
;;;;    country-kit
;;;;    rockabilly
;;;;
;;;;  key   zone 
;;;;  033 - Z01  Main Kick (Tuned) 
;;;;  034 - Z01  Main Kick (Tuned) 
;;;;  035 - ZO1  Main Kick (Tuned) 
;;;;  036 - ZO1  Main Kick (Tuned) 
;;;;  037 - Z04  Aux Snare 1
;;;;  038 - Z03  Main Snare
;;;;  039 - Z05  Aux Snare 2
;;;;  040 - Z19  Tom (Tuned)
;;;;  041 - Z11  Tom (Tuned)
;;;;  042 - Z07  HH Closed
;;;;  043 - Z12  Tom (Tuned)
;;;;  044 - Z08  HH Open 
;;;;  045 - Z19  Tom (Tuned)
;;;;  046 - Z10  Percussion
;;;;  047 - Z19  Tom (Tuned)
;;;;  048 - Z15  Choke Cymbol (or other percussion)
;;;;  049 - Z16  Cymbal 1 (Crash or ride)
;;;;  050 - Z17  Aux Ride
;;;;  051 - Z18  Cymbal 2 (Crash or Ride)
;;;;  052 - Z09  HH Stack (Controller A)
;;;;  053 - Z02  Kick (Nontransposed)
;;;;  054 - Z02  Kick (Nontransposed)
;;;;  055 - Z02  Kick (Nontransposed)
;;;;  056 - Z02  Kick (Nontransposed)
;;;;  057 - Z02  Kick (Nontransposed)
;;;;  058 - Z02  Kick (Nontransposed)
;;;;  059 - Z06  Snare (Nontransposed)
;;;;  060 - Z06  Snare (Nontransposed)
;;;;  061 - Z06  Snare (Nontransposed)
;;;;  062 - Z06  Snare (Nontransposed)
;;;;  063 - Z06  Snare (Nontransposed)
;;;;  064 - Z06  Snare (Nontransposed)
;;;;  065 - Z21  HH Stomp
;;;;  066 - Z21  HH Stomp
;;;;  067 - Z22  HH Shut
;;;;  068 - Z23  HH 2/3 Open
;;;;  069 - Z23  HH 2/3 Open
;;;;  070 - Z24  HH Open
;;;;  071 - Z24  HH Open
;;;;  072 - Z14  Tuned tom, bass or other 
;;;;  073 - Z14  Tuned tom, bass or other 
;;;;  074 - Z14  Tuned tom, bass or other
;;;;  075 - Z14  Tuned tom, bass or other
;;;;  076 - Z14  Tuned tom, bass or other
;;;;  077 - Z14  Tuned tom, bass or other
;;;;  078 - Z14  Tuned tom, bass or other
;;;;  079 - Z14  Tuned tom, bass or other
;;;;  080 - Z14  Tuned tom, bass or other
;;;;  081 - Z14  Tuned tom, bass or other
;;;;  082 - Z14  Tuned tom, bass or other
;;;;  083 - Z14  Tuned tom, bass or other
;;;;  084 - Z14  Tuned tom, bass or other
;;;;  085 - Z14  Tuned tom, bass or other
;;;;  086 - Z14  Tuned tom, bass or other
;;;;  087 - Z14  Tuned tom, bass or other
;;;;  088 - Z14  Tuned tom, bass or other
;;;;  089 - Z14  Tuned tom, bass or other
;;;;  090 - Z14  Tuned tom, bass or other
;;;;  091 - Z14  Tuned tom, bass or other
;;;;  092 - Z14  Tuned tom, bass or other
;;;;  093 - Z14  Tuned tom, bass or other
;;;;  094 - Z14  Tuned tom, bass or other
;;;;  095 - Z14  Tuned tom, bass or other
;;;;  096 - Z14  Tuned tom, bass or other
;;;;  097 - Z14  Tuned tom, bass or other
;;;;  098 - Z14  Tuned tom, bass or other

(in-package :cyco)

(let ((kick-map
       (keymap 'pkick 
               '((A  34)
                 (B  33)
                 (C  35)
                 (D  36))))
      (snare-map
       (keymap 'psnare 
               '((A       38 "Main Snare")
                 (B       37 "Alternate Snare 1")
                 (C       39 "Alternate Snare 2")
                 (D       46 "Percussion")
                 (X       38 "MOR2 Compatibility --> A")
                 (RIM     46 "MOR2 Compatibility --> D") 
                 (CRACK   46 "MOR2 Compatibility --> D")
                 (EDGE    37 "MOR2 Compatibility --> B")
                 (BOUNCE  39 "MOR2 Compatibility --> C")
                 (FLAM    39 "MOR2 Compatibility --> C")
                 (ROLL    39 "MOR2 Compatibility --> C")
                 (X2      38 "MOR2 Compatibility --> A")
                 (RIM2    46 "MOR2 Compatibility --> D")
                 (CRACK2  46 "MOR2 Compatibility --> D")
                 (EDGE2   37 "MOR2 Compatibility --> B"))))
       (tom-map
        (keymap 'ptom
                '((A  40 "Low tom")
                  (B  41)
                  (C  43)
                  (D  45)
                  (E  47 High tom")
                  (F  47 MOR2 Compatibility --> E")
                  (A-FLAM     40 "MOR2 Compatibility")
                  (B-FLAM     41 "MOR2 Compatibility")
                  (C-FLAM     43 "MOR2 Compatibility")
                  (D-FLAM     45 "MOR2 Compatibility")
                  (E-FLAM     47 "MOR2 Compatibility")
                  (F-FLAM     47 "MOR2 Compatibility")
                  (A-BOUNCE   40 "MOR2 Compatibility")
                  (B-BOUNCE   41 "MOR2 Compatibility")
                  (C-BOUNCE   43 "MOR2 Compatibility")
                  (D-BOUNCE   45 "MOR2 Compatibility")
                  (E-BOUNCE   47 "MOR2 Compatibility")
                  (F-BOUNCE   47 "MOR2 Compatibility"))))
       (hat-map
        (keymap 'phat
                '((X        42)
                  (OPEN     44)
                  (STOMP    65)
                  (SHUT     67)
                  (OPN      68 "2/3 open")
                  (STACK    52 "Mod wheel controlled?")
                  (STOMP2   66)
                  (OPN2     69) 
                  (OPEN2    70)
                  (OPEN3    71))))
       (cym-map
        (keymap 'pcym
                '((RIDE   50)
                  (A      49)
                  (B      51)
                  (C      (48 "Sometimes choke")))))
       (tuned-map 
        (reduced-keymap 'ptuned 72 89)))

  (param standard-procussion-kit
         (create-instrument 'standard-procussion-kit
                            :parent pro1
                            :transient nil
                            :program-number 0))

  (param pkick (create-instrument 'pkick
                                  :parent standard-procussion-kit
                                  :transient nil
                                  :keynumber-map kick-map))

  (param psnare (create-instrument 'psnare
                                   :parent standard-procussion-kit
                                   :transient nil
                                   :keynumber-map snare-map))
  
  (param ptom (create-instrument 'ptom
                                 :parent standard-procussion-kit
                                 :transient nil
                                 :keynumber-map tom-map))
  
  (param phat (create-instrument 'phat
                                 :parent standard-procussion-kit
                                 :transient nil
                                 :keynumber-map hat-map))
  
  (param pcym (create-instrument 'pcym
                                 :parent standard-procussion-kit
                                 :transient nil
                                 :keynumber-map cym-map))
  
  (param ptuned (create-instrument 'ptuned
                                   :parent standard-procussion-kit
                                   :transient nil
                                   :keynumber-map tuned-map))
  
  (defun standard-procussion-kit (kit-name)
    (let ((spec (cdr (assoc kit-name +PROCUSSION-PROGRAMS+))))
      (if (not (eq (second spec) 'standard))
          (let ((msg "~A is not a standard Procussion kit (using program 0)"))
            (cyco-warning (format nil msg kit-name))
            (setf spec '(0 NON-STANDARD))))
      (property! standard-procussion-kit :program-number (car spec))
      (format t "Using Standard Procussion kit '~A', program ~A~%"
              kit-name (car spec))))
  
  (defun ampitheater ()(standard-procussion-kit 'ampitheater))
  (defun mega-drums ()(standard-procussion-kit 'mega-drums))
  (defun rock-n-roll ()(standard-procussion-kit 'rock-n-roll))
  (defun palladium ()(standard-procussion-kit 'palladium))
  (defun jazz-drums ()(standard-procussion-kit 'jazz-drums))
  (defun metal-drums ()(standard-procussion-kit 'metal-drums))
  (defun rap-session ()(standard-procussion-kit 'rap-session))
  (defun ambient-rock ()(standard-procussion-kit 'ambient-rock))
  (defun acoustic-kit ()(standard-procussion-kit 'acoustic-kit))
  (defun rock-drums ()(standard-procussion-kit 'rock-drums))
  (defun house-machine ()(standard-procussion-kit 'house-machine))
  (defun fusion-stix ()(standard-procussion-kit 'fusion-stix))
  (defun space-drums ()(standard-procussion-kit 'space-drums))
  (defun hard-rock ()(standard-procussion-kit 'hard-rock))
  (defun stadium-rox ()(standard-procussion-kit 'stadium-rox))
  (defun dance-2000 ()(standard-procussion-kit 'dance-2000))
  (defun heavy-metal ()(standard-procussion-kit 'heavy-metal))
  (defun hip-hop ()(standard-procussion-kit 'hip-hop))
  (defun sluggo-drums ()(standard-procussion-kit 'sluggo-drums))
  (defun huge-room ()(standard-procussion-kit 'huge-room))
  (defun drum-dance ()(standard-procussion-kit 'drum-dance))
  (defun heavyosity ()(standard-procussion-kit 'heavyosity))
  (defun dance-club ()(standard-procussion-kit 'dance-club))
  (defun country-kit ()(standard-procussion-kit 'country-kit))
  (defun rockabilly ()(standard-procussion-kit 'rockabilly)) ) 
