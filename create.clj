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

; sensors
(bulkdef
  BUMPS_AND_WHEEL_DROPS 7,
  WALL_IR_SENSOR 8,
  CLIFF_LEFT 9,
  CLIFF_FRONT_LEFT 10,
  CLIFF_FRONT_RIGHT 11,
  CLIFF_RIGHT 12,
  VIRTUAL_WALL 13,
  LSD_AND_OVERCURRENTS 14,
  INFRARED_BYTE 17,
  BUTTONS 18,
  DISTANCE 19,
  ANGLE 20,
  CHARGING_STATE 21,
  VOLTAGE 22,
  CURRENT 23,
  BATTERY_TEMP 24,
  BATTERY_CHARGE 25,
  BATTERY_CAPACITY 26,
  WALL_SIGNAL 27,
  CLIFF_LEFT_SIGNAL 28,
  CLIFF_FRONT_LEFT_SIGNAL 29,
  CLIFF_FRONT_RIGHT_SIGNAL 30,
  CLIFF_RIGHT_SIGNAL 31,
  CARGO_BAY_DIGITAL_INPUTS 32,
  CARGO_BAY_ANALOG_SIGNAL 33,
  CHARGING_SOURCES_AVAILABLE 34,
  OI_MODE 35,
  SONG_NUMBER 36,
  SONG_PLAYING 37,
  NUM_STREAM_PACKETS 38,
  REQUESTED_VELOCITY 39,
  REQUESTED_RADIUS 40,
  REQUESTED_RIGHT_VELOCITY 41,
  REQUESTED_LEFT_VELOCITY 42,
  POSE 100,
  LEFT_BUMP 101,
  RIGHT_BUMP 102,
  LEFT_WHEEL_DROP 103,
  RIGHT_WHEEL_DROP 104,
  CENTER_WHEEL_DROP 105,
  LEFT_WHEEL_OVERCURRENT 106,
  RIGHT_WHEEL_OVERCURRENT 107,
  ADVANCE_BUTTON 108,
  PLAY_BUTTON 109)

; numerical contants
(bulkdef 
  BOT_RADIUS 258.0,
  STRAIGHT 32768)

;; low-level functions
;; ------------------------

(defn int->2twoscomp
  "Convert an integer to a high-value, low-value two's complement
  pair. Returns high value first, in a vec. 255_{10} == 0xff."
  [n]
  (let [value (if (< n 0) (+ (bit-shift-left 1 16) n) 
                          n)]
    (vec [(bit-and (bit-shift-right value 8) 255) 
          (bit-and value 255)])))

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
                   :sensors {}
                   :state :idle
                   :mode :safe
                   :pose {:x 0.0 :y 0.0 :theta 0.0})]
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
        rad (cond (> radius-mm 2000) STRAIGHT
                  (< radius-mm -2000) STRAIGHT
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
     (if (= vel-mm 0) (drive (* (Math/abs rad-sec) 
                                  (/ BOT_RADIUS 2))
                               0
                               dir)
                      (drive vel-mm (/ vel-mm (rad-sec)))))))
           

  


