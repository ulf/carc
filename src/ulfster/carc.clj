(ns ulfster.carc
  (:gen-class)
  (:require clojure.set)
  (:require clojure.contrib.seq-utils)
  (:use    compojure))

; Entities are the types of items on each border, center is true if a monestary is on the card
; Connections are between two borders of cities or streets, grass comes laster
; Name is the filename of the icon
(defstruct card :entities :center :connections :name)
; A field is a card on the playing field, including position and rotation
; Card is rotated :rotation * 90 degrees clockwise
(defstruct field :id :card :rotation :pos)
; A player avatar on the playing field
(defstruct icon :id :player :field :location :type)
; A player taking part in a game :code is used to limit access to the correct games
; Cookie needs to be set, :gamecode=:playercode
(defstruct player :name :code :icons :points :joined)
; One game running on the server, including the already placed cards (fields), the remaining
; cards and the players as well as turn information
(defstruct game :code :fields :cards :players :turn :stage :next)

(def games (ref {})) ;global games list

;Database of cards, incomplete
(def cards (concat
	    (repeat 3 (struct card [:s :c :s :g] nil [[0 2]] "D" ))
	    (repeat 5 (struct card [:c :g :g :g] nil [] "E" ))
	    (repeat 3 (struct card [:s :c :s :s] nil [] "L" ))
	    ))


;Starting field, always the same card
(def fields (list (struct field 
		   1                                                 ; ID
		   (struct card [:s :c :s :g] nil [[0 2]] "D" )      ; Card
		   1                                                 ; Rotation x * 90deg clockwise
		   [0 0])))                                          ; Position

;Directional vectors [top right bottom left]
(def dirs [[0 -1] [1 0] [0 1] [-1 0]])

(defn addpoints [& pts]
  "Returns sum of two or more position vectors
   (addpoints [0 3] [1 1]) -> [1 4]"
  (vec (apply map + pts)))

(defn outerFields [fields]
  "Returns all adjacent fields of all existing fields"
  (for [f fields ;Loop through all fields
	d dirs]  ;For every field look in every direction
    (addpoints (f :pos) d)))

(defn freeFields [fields]
  "Returns the outerFields which are not occupied"
  (vec
   (clojure.set/difference
    ;Take the adjacent fields
    (set (outerFields fields))
    ;But without fields which are already occupied
    (set (for [f fields] (f :pos))))))

(defn getField [fields pos]
  "Returns the field structure residing at the specified position or nil"
  (first
   (filter
    #(= pos (:pos %))
    fields)))

(defn getStructure [f]
  "Return a vector containing the entitytypes and the rotation of the card in the specified field"
     (if
	 (nil? f)
       nil
       (vec (list ((f :card) :entities) (f :rotation)))))

(defn getPattern [field allfields]
  "Returns pattern necessary to put a card in the specified spot.
   Patterns are 4-el vectors containing either nil or an entitiy keyword as elements"
  (let [neighbors (for [d dirs] (addpoints d field)) ; neighbors are locations of all adjacent fields
	nFields (map #(getField allfields %) neighbors) ; get field structures of that locations
	borders (zipmap ; create an index-keyed map of the structural items
		 (range 4)
		 (map getStructure nFields))
	p (for [k (sort (keys borders))] ; return 4-vec pattern to fit in the spot at field
	    (if (nil? (borders k))
	      nil
	      (((borders k) 0) (mod (+ 2 k (- ((borders k) 1))) 4))))
	]
    (vec p)))

(defn matchPattern [c p]
  "Calculates whether the card c can fit into pattern p"
  (if (and (empty? c) (empty? p))
    true
    (if (or (= (first c) (first p)) (= nil (first p)))
      (matchPattern (rest c) (rest p))
      false)))

(defn matchable [card pattern]
  "Returns list of rotation indexes of pattern, which are compatible with the card entities"
  (let [patterns (zipmap
		  (range 4)
		  (clojure.contrib.seq-utils/rotations pattern))] ; rotates counterclockwise
    (filter #(not (nil? %))
	    (for [k (sort (keys patterns))]
	      (if
		  (matchPattern card (patterns k))
		k
		nil)))))

(defn possibleMoves [fields card]
  "Returns a list of all free fields where the card fits and the rotation values that qualify"
  (if card
    (let [free (freeFields fields)
	  entities (card :entities)]
      (filter #(not (empty? (% 1))) ; only allow fields with at least one fitting possibility
	      (map #(vec (list (% 0) (matchable entities (% 1))))
		   (for [f free]
		     (vec (list f (getPattern f fields)))))))
    ()))
  
(defn boundaries [fields]
  "Returns the bounding rectangle of all fields, considering their position
   with one field padding"
  (loop [flds fields
	 xl 0
	 xh 0
	 yl 0
	 yh 0]
    (if
	(empty? flds)
      [(range (dec xl) (+ 2 xh)) (range (dec yl) (+ 2 yh))]
      (let [ f (first flds)	    
	    [x y] (f :pos)]
	(recur (rest flds) (min xl x) (max xh x) (min yl y) (max yh y))))))

(defn showField [game usercookie]
  "Displays html version of game's current playing field"
  (let [fields @(game :fields)
	card (first @(game :cards))
	hash (game :code)
	turn @(game :turn)
	[xs ys] (boundaries fields)
	possible (possibleMoves fields card)]
    [:table {:cellpadding "0" :cellspacing "0"}
     (for [y ys]
       [:tr 
	(for [x xs]
	  (let [field (getField fields [x y])
		moves (nth (first (filter #(= [x y] (% 0)) possible)) 1 false)]
	    [:td {:width 105 :height 105}
	     (if field
	       [:img {:src (str "/static/icons/" (:name (:card field)) ".JPG") :class (str "rot" (:rotation field)) :width 103 :height 103}]   
	       (if (and moves (= turn usercookie))
		 [:div {:width 103 :height 103 :style "background-color: #EFEFEF"}
		  (for [m moves]
		    [:a {:href (str "/board/" hash "/move/" x "/" y "/" m)}
		     [:img {:src (str "/static/icons/" (:name card) ".JPG") :class (str "rot" m) :width 45 :height 45 :style "padding: 2px" :border 0}]]
		    )
		  ]
		[:div {:width 103 :height 103}])
	       )]
	    ))])]))


(defn addPlayer [game player]
  "Add a player to the specified game"
  (dosync
   (ref-set (game :players) (assoc @(game :players) (player :code) player))
   (let [k (keys @(game :players))]
     (ref-set (game :next) (zipmap k (take (count k) (drop 1 (cycle k))))))
   (if (not @(game :turn))
     (ref-set (game :turn) (player :code)))))

(defn initGame [games]
  "This initializes a new game, stores it in the global games map and returns the new game"
  (let [code (.substring (str (rand)) 2)
	game (struct game code (ref fields) (ref (clojure.contrib.seq-utils/shuffle cards)) (ref {}) (ref false) (ref 0) (ref {}))]
    (dosync (ref-set games (assoc @games (keyword code) game)))
    game))

(defn createFieldForm []
  "Display form to create a new game"
  (html
   [:head
    (include-css "/static/style.css")
    ]
   [:body
    [:form {:action "/new" :method "POST"}
     (for [x (range 1 4)] [:div
     [:p (str "Player " x)]
     [:input {:name (str "user[]") :type "text"}]
     [:p "Email"]
     [:input {:name (str "email[]") :type "text"}][:br][:br]
     ])
     [:input {:type "submit" :value "start"}]
    ]]))

(defn sendInvite [name email code gameHash]
  "Returns invitation string to be sent as email"
  (format "
To: %s
Hello %s,

you got an invite to play Carc:
http://localhost:8000/join/%s/%s
Have Fun!
"
	    email name gameHash code))
 
(defn createField [params]
  "Creates new Gameboard and adds the players to the game"
  (let [game (initGame games)
	users (zipmap (filter #(> (count (.trim %)) 0) (params (keyword "user[]"))) (params (keyword "email[]")))]
    (for [name (keys users)]
      (let [code (.substring (str (rand)) 2)]
	(addPlayer game (struct player name code (ref {}) (ref 0) (ref 0)))
	(html [:p (sendInvite name (users name) code (game :code))])
	))))

(defn joinGame [gameHash cookieHash]
  "Lets a player join a game he has been invited to"
  (let [game (@games (keyword gameHash))]
    (if
	(empty? game)
      (html "Game does not exist")
      (let [player (@(game :players) cookieHash)]
	(if (= 1 @(player :joined))
	  (html "Already joined")
	  (do	    
	    (dosync (alter (player :joined) inc))
	    {:status 200 :headers ((compojure.http.helpers/set-cookie gameHash cookieHash "path" "/") :headers) :body (html [:a {:href (str "/board/" gameHash)} "Congrats!"])} ))))))


(defn showMenu [game usercookie]
  "Return HTML of overview column"
  (let [players @(game :players)
	me (players usercookie)
	card (first @(game :cards))
	turn @(game :turn)]
    [:table
     [:tr [:td {:colspan 2} (str "Hallo " (me :name))]]
     (for [k (keys players)]
       (let [p (players k)]
	 [:tr
	  (if (= (p :code) turn)
	    [:td [:b (p :name)]]
	    [:td (p :name)])
	  [:td @(p :points)]]))
     (if card
       (html [:tr [:td {:colspan "2"} "Card"]]
	     [:tr [:td {:colspan "2"} [:img {:src (str "/static/icons/" (:name card) ".JPG") :width 103 :height 103}]]])
       [:tr [:td {:colspan "2"} "Finished"]])]))
  
(defn showBoard [game usercookie]
  "Return HTML code of entire board, including menu"
  (if game
    (html
     [:head
      (include-css "/static/style.css")
      ]
     [:body
      [:table
       [:tr
	[:td {:valign "top"} (showMenu game usercookie)]
	[:td {:valign "top"} (showField game usercookie)]
	]]])
    "game not found"))

(defn move [game usercookie x y rotation]
  "Make a move, set card at specified position and rotation
   if it's the requeting players turn"
  (if (and (= @(game :turn) usercookie)
	   (= @(game :stage) 0))
    (let [c (first @(game :cards))]
      (dosync
       (alter (game :fields) conj (struct field (inc (count @(game :fields))) c rotation [x y]))
       (alter (game :cards) rest)
       (ref-set (game :turn) (@(game :next) usercookie))
       (redirect-to (str "/board/" (game :code)))))
    (redirect-to (str "/board/" (game :code)))))
	   
(defroutes carcassonne
  (GET  "/"         (showField fields)) 
  (GET  "/new"      (createFieldForm))
  (POST "/new"      (createField params))
  (GET  "/join/:hash/:cookie" (joinGame (params :hash) (params :cookie)))
  (GET  "/board/:hash"  (showBoard (@games (keyword (params :hash))) (cookies (keyword (params :hash)))))
  (GET  "/board/:hash/move/:x/:y/:rot"  (move (@games (keyword (params :hash)))
					      (cookies (keyword (params :hash)))
					      (Integer. (params :x))
					      (Integer. (params :y))
					      (Integer. (params :rot))))
  (GET  "/static/*" (serve-file "../res" (params :*)))
  (ANY "*"  404))
 
(defn -main [& args]
  (run-server {:port 8000} "/*" (servlet carcassonne)))


(defn startserv []
  "Used for debugging, start a server and save it in a variable"
  (def server (-main)))

(defn stopserv []
  "Stopps saved server"
  (stop server))