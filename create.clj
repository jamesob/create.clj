;;; create.clj
;;; adapted (in part) from create.py, by Zach Dodds et al.
;;;
;;;     james o'beirne
;;;           george mason university
;;;                  dec 2009
(ns create 
  (:import 
     (java.net Socket)
     (java.io DataInputStream DataOutputStream)))

;; socket/serial communication
;; ---------------------------

(defstruct socket-struct 
           :java-obj :send :recv :close)

(defn- make-recvfn
  "Return a closure which reads bytes from the serial-socket.
  For use only when building a socket struct."
  [sockobj]
  (let [fromsock (new DataInputStream (.getInputStream sockobj))]
    (fn
      ([] (.readByte fromsock))
      ([numbytes] (for [i (range 0 numbytes)] (.readByte fromsock))))))

(defn- make-sendfn
  "Return a closure which sends bytes to the serial-socket.
  For use only when building a socket struct."
  [sockobj]
  (let [tosock (new DataOutputStream (.getOutputStream sockobj))]
    (fn [opsseq]
      (dorun (map #(doto tosock (.writeByte %) (.flush)) opsseq)))))
      

(defn make-socket
  "Returns a socket struct which is designed to communicate
  with serialdaemon.c, providing a conduit to the serial port."
  [host port]
  (let [sockobj (new Socket host port)
        recvfn (make-recvfn sockobj)
        sendfn (make-sendfn sockobj)
        closefn (fn [] (.close sockobj))]
    (struct socket-struct sockobj sendfn recvfn closefn)))

;; constants galore
;; ----------------------------

(defmacro bulkdef
  "Writes a def for each pair given."
  [& pairs]
  `(do ~@(map (fn [[a b]] `(def ~a ~b)) (partition 2 pairs))))

; opcodes
(bulkdef
  START   128,
  BAUD    129, ; + 1 byte
  CONTROL 130,
  SAFE    131,
  FULL    132,
  POWER   133,
  SPOT    134,
  CLEAN   135,
  COVER   135,
  MAX     136,
  DEMO    136,
  DRIVE   137, ; + 4 bytes
  MOTORS  138, ; + 1 byte
  LEDS    139, ; + 3 bytes
  SONG    140, ; + 2n + 2 bytes, n = |notes|
  PLAY    141, ; + 1 byte
  SENSORS 142, ; + 1 byte
  FORCESEEKINGDOCK 143,
  DRIVEDIRECT 145,
  STREAM  148,
  QUERYLIST 149,
  PAUSERESUME 150,
  SCRIPT  152,
  ENDSCRIPT 153,
  WAITDIST 156,
  WAITANGLE 157,
  OFF_MODE 0,
  PASSIVE_MODE 1,
  SAFE_MODE 2,
  FULL_MODE 3)

;; sensor wrangling
;; ---------------------------

(defstruct sens-struct
           :opcode :groupid :bytesize :bit :value)

(defn- build-sensmap
  "Builds up a hashmap which contains a sens-struct for each
  sensor on Create."
  [& sensinfo]
  (loop [sensmap {}
         sensors (partition 5 sensinfo)
         s (first sensors)]
    (if sensors
      (recur (assoc sensmap (first s) 
                            (struct-map sens-struct
                                    :opcode (nth s 1)
                                    :groupid (nth s 2)
                                    :bytesize (nth s 3)
                                    :bit (nth s 4)))
             (next sensors)
             (first (next sensors)))
      sensmap)))

(defn init-sensors
  "Initialize the sensmap data structure with values as specified
  by the Create OI. Returns a hashmap populated by sens-structs."
  []
  (build-sensmap
    ; name/key                     opcode groupid bytesize bit
    "BUMPS_AND_WHEEL_DROPS"        7      1       1        nil,
    "WALL_IR_SENSOR"               8      1       1        nil,
    "CLIFF_LEFT"                   9      1       1        nil,
    "CLIFF_FRONT_LEFT"            10      1       1        nil,
    "CLIFF_FRONT_RIGHT"           11      1       1        nil,
    "CLIFF_RIGHT"                 12      1       1        nil,
    "VIRTUAL_WALL"                13      1       1        nil,
    "LSD_AND_OVERCURRENTS"        14      1       1        nil,
    "INFRARED_BYTE"               17      2       1        nil,
    "BUTTONS"                     18      2       1        nil,
    "DISTANCE"                    19      2       2        nil,
    "ANGLE"                       20      2       2        nil,
    "CHARGING_STATE"              21      3       1        nil,
    "VOLTAGE"                     22      3       2        nil,
    "CURRENT"                     23      3       2        nil,
    "BATTERY_TEMP"                24      3       1        nil,
    "BATTERY_CHARGE"              25      3       2        nil,
    "BATTERY_CAPACITY"            26      3       2        nil,
    "WALL_SIGNAL"                 27      4       2        nil,
    "CLIFF_LEFT_SIGNAL"           28      4       2        nil,
    "CLIFF_FRONT_LEFT_SIGNAL"     29      4       2        nil,
    "CLIFF_FRONT_RIGHT_SIGNAL"    30      4       2        nil,
    "CLIFF_RIGHT_SIGNAL"          31      4       2        nil,
    "CARGO_BAY_DIGITAL_INPUTS"    32      4       1        nil,
    "CARGO_BAY_ANALOG_SIGNAL"     33      4       2        nil,
    "CHARGING_SOURCES_AVAILABLE"  34      4       1        nil,
    "OI_MODE"                     35      5       1        nil,
    "SONG_NUMBER"                 36      5       1        nil,
    "SONG_PLAYING"                37      5       1        nil,
    "NUM_STREAM_PACKETS"          38      5       1        nil,
    "REQUESTED_VELOCITY"          39      5       2        nil,
    "REQUESTED_RADIUS"            40      5       2        nil,
    "REQUESTED_RIGHT_VELOCITY"    41      5       2        nil,
    "REQUESTED_LEFT_VELOCITY"     42      5       2        nil,
    "POSE"                       100    nil     nil        nil,
    "LEFT_BUMP"                  101    nil     nil          1,
    "RIGHT_BUMP"                 102    nil     nil          0,
    "LEFT_WHEEL_DROP"            103    nil     nil          3,
    "RIGHT_WHEEL_DROP"           104    nil     nil          2,
    "CENTER_WHEEL_DROP"          105    nil     nil          4,
    "LEFT_WHEEL_OVERCURRENT"     106    nil     nil          4,
    "RIGHT_WHEEL_OVERCURRENT"    107    nil     nil          3,
    "ADVANCE_BUTTON"             108    nil     nil          2,
    "PLAY_BUTTON"                109    nil     nil          0))

; numerical constants
(bulkdef 
  BOT_RADIUS 258.0,
  STRAIGHT 32768)

;; low-level functions
;; ------------------------

(defn int->2twoscomp
  "Convert an integer to a high-value, low-value two's complement
  pair. Returns high value first, in a vec. 255_{10} == 0xff."
  [value]
  (vec [(bit-and (bit-shift-right value 8) 255) 
        (bit-and value 255)]))

;; instantiating the Create
;; ------------------------

(defstruct robot-struct 
           :send :recv :sensors :state :mode :pose)

(def bot (ref (struct robot-struct)))
(declare write receive)

(defn init
  "Opens communications with serialdaemon.c and thus
  with the Create itself; sets bot to the resulting
  robot-struct. Boots Create into safe mode."
  [port]
  (let [socket (make-socket "localhost" port)
        r-struct (struct-map robot-struct
                   :send (socket :send)
                   :recv (socket :recv)
                   :sensors {})]
    (dosync (ref-set bot r-struct))
    (def write (@bot :send))
    (def receive (@bot :recv))
    (write [START])
    (Thread/sleep 200)
    (write [SAFE])
    (println (format "Booted Create into safe mode on port %d." port))))


;; movement functions
;; ------------------

(defn- drive
  "Internal function. Backbone of go, et al."
  [vel-mm radius-mm & turn-dir]
  (let [vel (cond (> vel-mm 500) 500 ; cap values
                  (< vel-mm -500) -500
                  :else vel-mm)
        rad (cond (> (Math/abs radius-mm) 2000) STRAIGHT
                  (= radius-mm 0) (if (= "CW" turn-dir) -1 1)
                  :else radius-mm)]
    (dorun (map #'write [[DRIVE]
                         (int->2twoscomp (int vel))
                         (int->2twoscomp (int rad))]))))
(defn go
  "(go) stops the robot. (go foo bar) causes the robot to move
  forward at foo cm/sec and turn at bar deg/sec. (go foo) just
  causes the robot to move forward at foo cm/sec."
  ([] (drive 0 0)) ; stop
  ([cm-sec] (drive (* 10.0 cm-sec) STRAIGHT)) ; drive straight
  ([cm-sec deg-sec] ; curved path OR stationary turn
   (let [rad-sec (Math/toRadians deg-sec)
         dir (if (>= rad-sec 0) "CCW" 
                                "CW")
         vel-mm (* 10.0 cm-sec)]
     (if (= vel-mm 0) (drive (* (Math/abs rad-sec) (/ BOT_RADIUS 2))
                             0
                             dir)
                      (drive vel-mm (/ vel-mm (rad-sec)))))))
           

  


