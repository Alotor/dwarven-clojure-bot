(ns dwarven.bot.dummy
  (:require [clojure.data.json :as json])
  (:import [java.nio.channels SocketChannel])
  (:import [java.nio ByteBuffer])
  (:import [java.nio.charset Charset CharsetDecoder])
  (:import [java.net InetSocketAddress])
)

;; Flag to activate log for input-output
(def debug-messages true)

;; Bots names
(def dwarves-names [
    "Horse",
    "Pikachu",
    "The Pig",
    "Tiger",
    "Chimpokomon"
])

;; Aux functions
(defn multimap 
  "Applies a list of functions to a list of values. Returns a list
   with the same size of the largest vector.
   i.e. (multimap [#(* 2 %) #(+ 10 %)] [2, 10]) => (4 20)" 
  [funVec valVec]
  (loop [functions funVec values valVec result []]
    (let 
      [ curFun (first functions)
        curVal (first values)]
      (if (and curFun curVal)
        (recur (rest functions) (rest values) (conj result (curFun curVal)))
        result))))

;; Socket write/read
(defn parse-json 
  "Method that parses the json string messages from the server. Checks for errors" 
  [message]
  (when debug-messages (println "Receive< " message))
  (try 
    (json/read-str message :key-fn keyword)
    (catch Exception e 
      (println "ERROR: " message) 
      (hash-map :type "error" :message (str "FAILED " message)))))

(defn write-json!
  "Writes a json message to the input SocketChannel" 
  [socket message] 
  (let 
    [ buf (ByteBuffer/allocate 1024)
      text (json/json-str message)]
    (.clear buf)
    (.put buf (.getBytes text))
    (.flip buf)
    (when debug-messages (println "Sending>" text))
    (.write socket buf)))

(defn read-json!
  "Read from the socket an transform the input into json"
  [socket]
  (let 
    [ buf (ByteBuffer/allocate 2048)
      decoder (.newDecoder (Charset/forName "UTF-8"))]
    (.read socket buf)
    (.flip buf)

    ;; The buffer contains N messages separated by end-of-line
    ;; Separate the messages and returns an array
    (let
      [ charbuf (.decode decoder buf)
        messages (clojure.string/split-lines (.toString charbuf))]
      (map parse-json messages))))

;; The following program it's a base implementation for the dwarven-tavern bot
(defn move-direction [curCoord targetCoord] ())

(defn attack-strategy [info bot] ())

(defn defense-strategy [info bot] ())

;; Create simulation
(defn create-simulation [] { :type "create-simulation" })

;; Join simulation
(defn join-simulation [nick simulationId] {
    :type "join-simulation"
    :nick nick
    :simulationId simulationId
    :names dwarves-names })

;; Functions to process the turn
(defn team-coords [world team]
  [{:x 1 :y 1}, {:x 2 :y 2}])

;; Given a sequence of the possition for the n bots build the next turn

;; Dummy handler
(defmulti dummy-bot-handler :type)

(defmethod dummy-bot-handler "game-info" [message info] 
  (println message) 
  (hash-map :info message))

(defmethod dummy-bot-handler "turn" [message info] 
  ; (hash-map :info info :actions [{:botId 6 :type "MOVE" :direction "SOUTH"}]))
  (let [ team (keyword (:team info))
         oponent (if (= team :team1) :team2 :team1)
         teamBarrelPosition (-> message :state :barrels team)
         oponentBarrelPosition (-> message :state :barrels oponent) 
         teamBots (-> message :state team)
         oponentBots (-> message :state team)
         info (hash-map :barrel teamBarrelPosition :opponentBarrel oponentBarrelPosition)
         attack (partial attack-strategy info)
         defense (partial defense-strategy info)
         strategies [attack attack attack defense defense]
        ]
    (hash-map :info info :actions (multimap strategies teamBots))))

(defmethod dummy-bot-handler "error" [message info] 
  (hash-map :info info))

(defmethod dummy-bot-handler "score" [message info] 
  (println "SCORE: " message)
  (hash-map :info info))

(defmethod dummy-bot-handler "loss-game" [message info] 
  (println "LOSS!!")
  (hash-map :end true))

(defmethod dummy-bot-handler "win-game" [message info] 
  (println "WIN!!")
  (hash-map :end true))

;; Main loop
(defn main-loop [handler]
  (let [ socket (SocketChannel/open)]
    (.connect socket (InetSocketAddress. 9000))
    (write-json! socket (join-simulation "Test" nil))
    (loop [ lastResult nil, messages (read-json! socket)]
       (when (seq (:actions lastResult)) 
         (write-json! socket {:type "player-turn" :actions (:actions lastResult)}))

       (when (= (:type (first messages)) "error") 
         (println (first messages))
         (.close socket))

       (when (and (not (:fatal (first messages))) (not= (:type (first messages)) "error") (not (:end lastResult)))
         (if (first messages) 
           (recur (handler (first messages) (:info lastResult)) (rest messages))
           (recur (dissoc lastResult :actions) (read-json! socket))))
    );loop
  );let
);defn

(defn -main [& args] 
  (main-loop dummy-bot-handler))
