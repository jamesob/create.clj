;;; create.clj
;;; a few parts yanked from create.py, by Zach Dodds et al.
;;;
;;;     james o'beirne
;;;           george mason university
;;;                  dec 2009
(ns create 
  (:import 
   (java.net Socket)
   (java.io DataInputStream DataOutputStream)))

;; language extensions
;; ---------------------------

(defn map-filter 
  "Returns a filtered map in the original order."
  [fnc map]
  (into (sorted-map) (reverse (filter fnc map))))

;; socket/serial communication
;; ---------------------------

(defstruct socket-struct 
  :java-obj :wobj :robj :wfn :rfn :close)

(defn- make-recvfn
  "Return a closure which reads bytes from the serial-socket.
  For use only when building a socket struct."
  [robj]
  (fn
    ([] (.readByte robj)) 
    ([numbytes] (force (for [i (range 0 numbytes)] (.readByte robj))))))


(defn- make-sendfn
  "Return a closure which sends bytes to the serial-socket.
  For use only when building a socket struct. Accepts collections
  or scalars."
  [wobj]
  (let [sendit (fn [byte] (doto wobj (.writeByte byte) (.flush)))]
    (fn [opsseq]
      (if (coll? opsseq)
	(dorun (map sendit opsseq))
	(sendit opsseq)))))

(defn- make-socket
  "Returns a socket struct which is designed to communicate
  with serialdaemon.c, providing a conduit to the serial port.
  To be used in init."
  [host port]
  (let [sockobj (new Socket host port)
	robj (new DataInputStream (.getInputStream sockobj))
	wobj (new DataOutputStream (.getOutputStream sockobj))
        recvfn (make-recvfn robj)
        sendfn (make-sendfn wobj)
        closefn (fn [] (.close sockobj))]
    (struct socket-struct sockobj wobj robj sendfn recvfn closefn)))

;; constants galore
;; ----------------------------

(defmacro bulkdef
  "Writes a def for each pair given."
  [& pairs]
  `(do ~@(map (fn [[a b]] `(def ~a ~b)) (partition 2 pairs))))

; opcodes
(bulkdef
  START            128,
  BAUD             129, ; + 1 byte
  CONTROL          130,
  SAFE             131,
  FULL             132,
  POWER            133,
  SPOT             134,
  CLEAN            135,
  COVER            135,
  MAX              136,
  DEMO             136,
  DRIVE            137, ; + 4 bytes
  MOTORS           138, ; + 1 byte
  LEDS             139, ; + 3 bytes
  SONG             140, ; + 2n + 2 bytes, n = |notes|
  PLAY             141, ; + 1 byte
  SENSORS          142, ; + 1 byte
  FORCESEEKINGDOCK 143,
  DRIVEDIRECT      145,
  STREAM           148,
  QUERYLIST        149,
  PAUSERESUME      150,
  SCRIPT           152,
  ENDSCRIPT        153,
  WAITDIST         156,
  WAITANGLE        157,
  OFF_MODE           0,
  PASSIVE_MODE       1,
  SAFE_MODE          2,
  FULL_MODE          3)

; numerical constants
(bulkdef 
  BOT_RADIUS 258.0,
  STRAIGHT 32768)  


;; low-level byte functions
;; ------------------------

(defn truncate-bitstr 
  "Truncate a bitstring to length n."
  [bitstr n]
  (reduce str (reverse (take n (reverse bitstr)))))


(defn pad-bitstr
  "Pad bitstring with n leadings."
  [bitstr n leading]
  (str (reduce str (take n (cycle [leading]))) bitstr))

(defn int->2twoscomp
  "Convert an integer to a high-value, low-value two's complement
  pair. Returns high value first, in a vec. 255_{10} == 0xff."
  [value]
  (vec [(bit-and (bit-shift-right value 8) 255) 
        (bit-and value 255)]))

(defn twobytes->uint
  "Given 2 unsigned bytes, returns an int."
  [[a b]]
  (bit-or (bit-shift-left a 8) b))

(defn twobytes->sint
  "Given 2 signed bytes, returns an int."
  [[a b]]
  (let [bitstr (Integer/toBinaryString (twobytes->uint [a b]))
	trunc (truncate-bitstr bitstr 15)]
    (Short/valueOf (str trunc) 2)))

(defn byte->sint
  "Given a signed byte, returns an int."
  [a]
  (let [abinstr (truncate-bitstr (Integer/toBinaryString a) 15)
	numpads (- 15 (count abinstr))
	fifteenbits (if (= 0 numpads) 
		      abinstr
		      (pad-bitstr abinstr numpads "0"))]
    (Short/valueOf fifteenbits 2)))

;; robot-struct
;; -----------------

(defstruct robot-struct 
  :sock :send :recv :sensors :state :mode :pose)

(defn write
  [bot & ops]
  (dorun (map (bot :send) ops)))

(defn flush-read-buff
  "Flush a robot's read buffer."
  [bot]
    (let [bytenum (.available ((bot :sock) :robj))]
      (println bytenum)
      ((bot :recv) bytenum)))

;; movement functions
;; ------------------

(defn- drive
  "Internal function. Backbone of go, et al."
  [bot vel-mm radius-mm & turn-dir]
  (let [vel (cond (> vel-mm 500) 500 ; cap values
                  (< vel-mm -500) -500
                  :else vel-mm)
        rad (cond (> (Math/abs radius-mm) 2000) STRAIGHT
                  (= radius-mm 0) (if (= "CW" turn-dir) -1 1)
                  :else radius-mm)]
    (write bot
	   DRIVE
	   (int->2twoscomp (int vel))
	   (int->2twoscomp (int rad)))))
(defn go
  "(go) stops the robot. (go foo bar) causes the robot to move
  forward at foo cm/sec and turn at bar deg/sec. (go foo) just
  causes the robot to move forward at foo cm/sec."
  ([bot] (drive bot 0 0)) ; stop
  ([bot cm-sec] (drive bot (* 10.0 cm-sec) STRAIGHT)) ; drive straight
  ([bot cm-sec deg-sec] ; curved path OR stationary turn
   (let [rad-sec (Math/toRadians deg-sec)
         dir (if (>= rad-sec 0) "CCW" 
                                "CW")
         vel-mm (* 10.0 cm-sec)]
     (if (= vel-mm 0) 
       (drive bot
	      (* (Math/abs rad-sec) (/ BOT_RADIUS 2))
	      0
	      dir)
       (if (= rad-sec 0)
	 (go bot cm-sec)
	 (drive bot vel-mm (/ vel-mm rad-sec)))))))
           
;; sensor wrangling
;; ---------------------------

(defstruct sens-struct
  :opcode :groupid :bytesize :bit :value)

(defn build-sensmap
  "Builds up a hashmap which contains a sens-struct for each
  sensor on Create."
  [& sensinfo]
  (loop [sensmap (sorted-map)
         sensors (partition 7 sensinfo)
         s (first sensors)]
    (if sensors
      (recur (assoc sensmap (first s) 
		    (struct-map sens-struct
		      :name (nth s 1)
		      :opcode (nth s 2)
		      :groupid (nth s 3)
		      :bytesize (nth s 4)
		      :bit (nth s 5)
		      :signed (nth s 6)))
             (next sensors)
             (first (next sensors)))
      sensmap)))

(defn init-sensors
  "Initialize the sensmap data structure with values as specified
  by the Create OI. Returns a hashmap populated by sens-structs."
  []
  (build-sensmap
    ;  key/name                       opcode groupid size bit  signed?
    1  "BUMPS_AND_WHEEL_DROPS"        7      1       1    nil  :u,
    2  "WALL_IR_SENSOR"               8      1       1    nil  :u,
    3  "CLIFF_LEFT"                   9      1       1    nil  :u,
    4  "CLIFF_FRONT_LEFT"            10      1       1    nil  :u,
    5  "CLIFF_FRONT_RIGHT"           11      1       1    nil  :u,
    6  "CLIFF_RIGHT"                 12      1       1    nil  :u,
    7  "VIRTUAL_WALL"                13      1       1    nil  :u,
    8  "LSD_AND_OVERCURRENTS"        14      1       1    nil  :u,
    9  "null"                       nil      1       2    nil  :u,
    10 "INFRARED_BYTE"               17      2       1    nil  :u,
    11 "BUTTONS"                     18      2       1    nil  :u,
    12 "DISTANCE"                    19      2       2    nil  :s,
    13 "ANGLE"                       20      2       2    nil  :s,
    14 "CHARGING_STATE"              21      3       1    nil  :u,
    15 "VOLTAGE"                     22      3       2    nil  :u,
    16 "CURRENT"                     23      3       2    nil  :u,
    17 "BATTERY_TEMP"                24      3       1    nil  :s,
    18 "BATTERY_CHARGE"              25      3       2    nil  :u,
    19 "BATTERY_CAPACITY"            26      3       2    nil  :u,
    20 "WALL_SIGNAL"                 27      4       2    nil  :u,
    21 "CLIFF_LEFT_SIGNAL"           28      4       2    nil  :u,
    22 "CLIFF_FRONT_LEFT_SIGNAL"     29      4       2    nil  :u,
    23 "CLIFF_FRONT_RIGHT_SIGNAL"    30      4       2    nil  :u,
    24 "CLIFF_RIGHT_SIGNAL"          31      4       2    nil  :u,
    25 "CARGO_BAY_DIGITAL_INPUTS"    32      4       1    nil  :u,
    26 "CARGO_BAY_ANALOG_SIGNAL"     33      4       2    nil  :u,
    27 "CHARGING_SOURCES_AVAILABLE"  34      4       1    nil  :u,
    28 "OI_MODE"                     35      5       1    nil  :u,
    29 "SONG_NUMBER"                 36      5       1    nil  :u,
    30 "SONG_PLAYING"                37      5       1    nil  :u,
    31 "NUM_STREAM_PACKETS"          38      5       1    nil  :u,
    32 "REQUESTED_VELOCITY"          39      5       2    nil  :s,
    33 "REQUESTED_RADIUS"            40      5       2    nil  :s,
    34 "REQUESTED_RIGHT_VELOCITY"    41      5       2    nil  :s,
    35 "REQUESTED_LEFT_VELOCITY"     42      5       2    nil  :s,
    36 "POSE"                       100    nil     nil    :na  :u,
    37 "LEFT_BUMP"                    7      1     nil      1  :u,
    38 "RIGHT_BUMP"                   7      1     nil      0  :u,
    39 "LEFT_WHEEL_DROP"              7      1     nil      3  :u,
    40 "RIGHT_WHEEL_DROP"             7      1     nil      2  :u,
    41 "CENTER_WHEEL_DROP"            7      1     nil      4  :u,
    42 "LEFT_WHEEL_OVERCURRENT"      14      1     nil      4  :u,
    43 "RIGHT_WHEEL_OVERCURRENT"     14      1     nil      3  :u,
    44 "ADVANCE_BUTTON"              18      2     nil      2  :u,
    45 "PLAY_BUTTON"                 18      2     nil      0  :u))

(defn- update-value
  "Updates some sensor's value in the sensor table."
  [foomap sensname newval]
  (assoc foomap sensname
	 (assoc (foomap sensname) :value newval)))

(defn derive-sensors
  [sensmap]
  sensmap)

(defn- parse-bytes
  "Iterates through list of bytes, decodes values accordingly."
  [bytes realsensors names data]
   (if bytes
     (let [currdata (first data)
	   currname (first names)]
       (if (= (currdata :bytesize) 1)
	 (if (= (currdata :signed) :u)
	     (parse-bytes (next bytes) 
			  (update-value realsensors 
					currname
					(first bytes))
			  (next names) (next data))
	     (parse-bytes (next bytes)
			  (update-value realsensors
					currname
					(byte->sint (first bytes)))
			  (next names) (next data)))
	 (if (= (currdata :signed) :u)
	     (parse-bytes (nnext bytes)
			  (update-value realsensors
					currname
					(twobytes->uint (take 2 bytes)))
			  (next names) (next data))
	     (parse-bytes (nnext bytes)
			  (update-value realsensors
					currname
					(twobytes->sint (take 2 bytes)))
			  (next names) (next data)))))
     realsensors))

(defn interpret-sensors
  "Interpret the bytes returned from update-sensors; can
either update entire sensor map (one argument) or do a selective
update (two arguments)."
  ([bot bytes]
     (let [updsens (parse-bytes bytes
				(map-filter (fn [[a b]] (not (b :bit))) 
					    (@bot :sensors))
				(keys (@bot :sensors))
				(vals (@bot :sensors)))]
       (dosync
	(ref-set bot
		 (merge @bot
			{:sensors (derive-sensors updsens)}))))
     nil)
  ([bot bytes bytes-map]))

(defn update-sensors!
  "Pulls sensor information from the Create. No arguments polls
  all sensors, but any number of strings can be specified for
  individual sensors."
  ([bot] ; grab all sensor data
     (do
       ; flush the read buffer
       (doall (flush-read-buff @bot))
       (Thread/sleep 100)
       (write @bot SENSORS 6)
       (interpret-sensors bot ((@bot :recv) 52))))
  ([bot & sensseq] ; lookup relevant opcodes, grab the data
     (let [opseq (map (fn [str] (((@bot :sensors) str) :opcode)) sensseq)
	   oplen (count sensseq)]
       (write @bot QUERYLIST oplen (distinct opseq))
       (interpret-sensors bot ((@bot :recv) oplen) sensseq))))

(defn print-sensors
  ([bot]
     (let [sensors (bot :sensors)
	   strs (map (fn [[k v]] (str (v :name) " : " (v :value) \newline)) sensors)]
       (println (reduce str strs))))
  ([bot & senslist]))
     


;; instantiating the Create
;; ------------------------


(defn init
  "Opens communications with serialdaemon.c and thus
  with the Create itself; sets bot to the resulting
  robot-struct. Boots Create into safe mode. Returns robot-struct."
  [port]
  (let [socket (make-socket "localhost" port)
        r-struct (struct-map robot-struct
		   ; send is any arity; each param can be scalar or a coll
		   :sock socket 
                   :send (socket :wfn)
                   :recv (socket :rfn)
                   :sensors (init-sensors))]
    (write r-struct START)
    (Thread/sleep 200)
    (write r-struct SAFE)
    (println (format "Booted Create into safe mode on port %d." port))
    ((r-struct :recv) 205) ; clear out read buffer
    r-struct))




  




