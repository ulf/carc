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
(defstruct icon :player :field :location :type)
; A player taking part in a game :code is used to limit access to the correct games
; Cookie needs to be set, :gamecode=:playercode
(defstruct player :name :code :icons :points :joined)
; One game running on the server, including the already placed cards (fields), the remaining
; cards and the players as well as turn information
(defstruct game :code :fields :cards :players :icons :turn :stage :next)

(def games (ref {})) ;global games list

(def DEFAULT_ICONS 6)

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

(defn outer-fields [fields]
  "Returns all adjacent fields of all existing fields"
  (for [f fields ;Loop through all fields
	d dirs]  ;For every field look in every direction
    (addpoints (f :pos) d)))

(defn free-fields [fields]
  "Returns the outer-fields which are not occupied"
  (vec
   (clojure.set/difference
    ;Take the adjacent fields
    (set (outer-fields fields))
    ;But without fields which are already occupied
    (set (for [f fields] (f :pos))))))

(defn get-field [fields pos]
  "Returns the field structure residing at the specified position or nil"
  (first
   (filter
    #(= pos (:pos %))
    fields)))

(defn get-structure [f]
  "Return a vector containing the entitytypes and the rotation of the card in the specified field"
     (if
	 (nil? f)
       nil
       (vec (list ((f :card) :entities) (f :rotation)))))

(defn get-pattern [field allfields]
  "Returns pattern necessary to put a card in the specified spot.
   Patterns are 4-el vectors containing either nil or an entitiy keyword as elements"
  (let [neighbors (for [d dirs] (addpoints d field)) ; neighbors are locations of all adjacent fields
	nFields (map #(get-field allfields %) neighbors) ; get field structures of that locations
	borders (zipmap ; create an index-keyed map of the structural items
		 (range 4)
		 (map get-structure nFields))
	p (for [k (sort (keys borders))] ; return 4-vec pattern to fit in the spot at field
	    (if (nil? (borders k))
	      nil
	      (((borders k) 0) (mod (+ 2 k (- ((borders k) 1))) 4))))
	]
    (vec p)))

(defn match-pattern [c p]
  "Calculates whether the card c can fit into pattern p"
  (if (and (empty? c) (empty? p))
    true
    (if (or (= (first c) (first p)) (= nil (first p)))
      (match-pattern (rest c) (rest p))
      false)))

(defn matchable [card pattern]
  "Returns list of rotation indexes of pattern, which are compatible with the card entities"
  (let [patterns (zipmap
		  (range 4)
		  (clojure.contrib.seq-utils/rotations pattern))] ; rotates counterclockwise
    (filter #(not (nil? %))
	    (for [k (sort (keys patterns))]
	      (if
		  (match-pattern card (patterns k))
		k
		nil)))))

(defn possible-moves [fields card]
  "Returns a list of all free fields where the card fits and the rotation values that qualify"
  (if card
    (let [free (free-fields fields)
	  entities (card :entities)]
      (filter #(not (empty? (% 1))) ; only allow fields with at least one fitting possibility
	      (map #(vec (list (% 0) (matchable entities (% 1))))
		   (for [f free]
		     (vec (list f (get-pattern f fields)))))))
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

(defn display-field [field]
  [:img {:src (str "/static/icons/" (:name (:card field)) ".JPG") :class (str "rot" (:rotation field)) :width 103 :height 103}])


(defn get-possible-moves [field]
  "Returns places where the player at turn can place a figure, same entities with existing connection
   are only counted once, return the already rotated "
  (let [c (:card field)
	conns (:connections c)
	ends (set (map #(nth % 1) conns))
	entities (clojure.set/difference
		  (set (map #(if (= (% 1) :g) nil (% 0))
			    (zipmap (range 4) (:entities c))))
		  #{nil})
	allowed (clojure.set/difference entities ends)]
    (map #(mod (+ % (:rotation field)) 4) allowed)
    ))

(def coords {0 [48 0] 1 [96 48] 2 [48 85] 3 [0 48]})

(defn display-current-field [field hash]
  (let [possible (get-possible-moves field)]
    (html [:div {:style "position: relative"} [:img {:src (str "/static/icons/" (:name (:card field)) ".JPG") :class (str "rot" (:rotation field)) :width 103 :height 103}]
	  (for [p possible]
	    [:div {:style (str "position: absolute; top: " ((coords p) 1) "; left: " ((coords p) 0) )}
	     [:a {:href (str "/board/" hash "/set/" p) :style "font-weight: bold; color: #00FF00; text-decoration: none"} p ]]
	    )])))

(defn show-field [game usercookie]
  "Displays html version of game's current playing field"
  (let [fields (game :fields)
	card (first (game :cards))
	hash (game :code)
	turn (game :turn)
	stage (game :stage)
	totalfields (count (game :fields))
	[xs ys] (boundaries fields)
	possible (possible-moves fields card)]
    [:table {:cellpadding "0" :cellspacing "0"}
     (for [y ys]
       [:tr 
	(for [x xs]
	  (let [field (get-field fields [x y])
		moves (nth (first (filter #(= [x y] (% 0)) possible)) 1 false)]
	    [:td {:width 103 :height 103}
	     (if field
	       ; If field is set display the card, plus the possibilities to place an icon if the stage is 1
	       (if (and (= stage 1)
			(= (field :id) totalfields)
			(= turn usercookie))
		 (display-current-field field hash)
		 (display-field field))
	       ; if the field is not set display possible moves there if suitable
	       (if (and moves (= turn usercookie) (= stage 0))
		 [:div {:width 103 :height 103 :style "background-color: #EFEFEF"}
		  (for [m moves]
		    [:a {:href (str "/board/" hash "/move/" x "/" y "/" m)}
		     [:img {:src (str "/static/icons/" (:name card) ".JPG") :class (str "rot" m) :width 45 :height 45 :style "padding: 2px" :border 0}]]
		    )
		  ]
		[:div {:width 103 :height 103}])
	       )]
	    ))])]))


(defn add-player [game player]
  "Pure function to add a player to a game struct"
  (let [players (assoc (game :players) (:code player) player)
	k (keys players)]
    (assoc game
      :players players
      :next (zipmap k (take (count k) (drop 1 (cycle k))))
      :turn (or (game :turn) (player :code))
      )))

(defn init-game [games]
  "This initializes a new game, stores it in the global games map and returns the new game"
  (let [code (.substring (str (rand)) 2)
	game (struct game code fields (clojure.contrib.seq-utils/shuffle cards) {} () false 0 {})]
    (dosync (alter games assoc (keyword code) game))
    game))

(defn create-field-form []
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

(defn send-invite [name email code gameHash]
  "Returns invitation string to be sent as email"
  (format "
To: %s
Hello %s,

you got an invite to play Carc:
http://localhost:8000/join/%s/%s
Have Fun!
"
	    email name gameHash code))
 
(defn create-field [params]
  "Creates new Gameboard and adds the players to the game"
  (let [game (init-game games)
	users (zipmap (filter #(> (count (.trim %)) 0) (params (keyword "user[]"))) (params (keyword "email[]")))]
    (for [name (keys users)]
      (let [code (.substring (str (rand)) 2)]
	(dosync (alter games assoc (keyword (game :code)) (add-player (games (keyword (game :code))) (struct player name code DEFAULT_ICONS 0 0))))
	(html [:p (send-invite name (users name) code (game :code))])
	))))

(defn player-join-game [game player]
  "Returns modified gamestruct with joined player"
  (assoc game :players (assoc (game :players) (player :code) (assoc player :joined 1))))

(defn join-game [gameHash cookieHash]
  "Lets a player join a game he has been invited to"
  (let [game (@games (keyword gameHash))]
    (if
	(empty? game)
      (html "Game does not exist")
      (let [player ((game :players) cookieHash)]
	(if (= 1 (player :joined))
	  (html "Already joined")
	  (do	    
	    (dosync (alter games assoc (keyword gameHash) (player-join-game game player)))
	    {:status 200 :headers ((compojure.http.helpers/set-cookie gameHash cookieHash "path" "/") :headers) :body (html [:a {:href (str "/board/" gameHash)} "Congrats!"])} ))))))


(defn show-menu [game usercookie]
  "Return HTML of overview column"
  (let [players (game :players)
	me (players usercookie)
	card (first (game :cards))
	turn (game :turn)]
    [:table
     [:tr [:td {:colspan 2} (str "Hallo " (me :name))]]
     (for [k (keys players)]
       (let [p (players k)]
	 [:tr
	  (if (= (p :code) turn)
	    [:td [:b (p :name)]]
	    [:td (p :name)])
	  [:td (p :points)]]))
     (if card
       (html [:tr [:td {:colspan "2"} "Card"]]
	     [:tr [:td {:colspan "2"} [:img {:src (str "/static/icons/" (:name card) ".JPG") :width 103 :height 103}]]])
       [:tr [:td {:colspan "2"} "Finished"]])]))
  
(defn show-board [game usercookie]
  "Return HTML code of entire board, including menu"
  (if game
    (html
     [:head
      (include-css "/static/style.css")
      ]
     [:body
      [:table
       [:tr
	[:td {:valign "top"} (show-menu game usercookie)]
	[:td {:valign "top"} (show-field game usercookie)]
	]]])
    "game not found"))

(defn make-move [game currentuser x y rotation]
  "Return modified game struct with move made"
  (let [c (first (game :cards))]
    (assoc game 
      :fields (conj (game :fields) (struct field (inc (count (game :fields))) c rotation [x y]))
      :cards (rest (game :cards))
      :stage 1
      )))

(defn move [game usercookie x y rotation]
  "Make a move, set card at specified position and rotation
   if it's the requeting players turn"
  (if (and (= (game :turn) usercookie)
	   (= (game :stage) 0))
      (dosync
       (alter games assoc (keyword (game :code)) (make-move game usercookie x y rotation)) 
       (redirect-to (str "/board/" (game :code))))
    (redirect-to (str "/board/" (game :code)))))

(defn current-field [game]
  (first (game :fields)))

(defn placed-icon [game currentuser place]
  (let [user ((game :players) currentuser)]
    (assoc game
      :players (assoc (game :players) currentuser
		      (assoc user :icons (dec (user :icons))))
      :icons (cons  (struct icon
			    currentuser
			    ((current-field game) :pos)
			    place
			    true)
		    (game :icons))
      :stage 0
      :turn ((game :next) currentuser)
      )))

(defn place-icon [game usercookie place]
  "Make a move, set icon at specified position and rotation
   if it's the requeting players turn"
  (if (and (= (game :turn) usercookie)
	   (= (game :stage) 1))
      (dosync
       (alter games assoc (keyword (game :code)) (placed-icon game usercookie place)) 
       (redirect-to (str "/board/" (game :code))))
    (redirect-to (str "/board/" (game :code)))))
	   
(defroutes carcassonne
  (GET  "/"         (show-field fields))
  (GET  "/new"      (create-field-form))
  (POST "/new"      (create-field params))
  (GET  "/join/:hash/:cookie" (join-game (params :hash) (params :cookie)))
  (GET  "/board/:hash"  (show-board (@games (keyword (params :hash))) (cookies (keyword (params :hash)))))
  (GET  "/board/:hash/move/:x/:y/:rot"  (move (@games (keyword (params :hash)))
					      (cookies (keyword (params :hash)))
					      (Integer. (params :x))
					      (Integer. (params :y))
					      (Integer. (params :rot))))
  (GET  "/board/:hash/set/:place"  (place-icon (@games (keyword (params :hash)))
					 (cookies (keyword (params :hash)))
					 (Integer. (params :place))))
  (GET  "/static/*" (serve-file "../../res" (params :*))) ; Path has to be adjusted when developing in SLIME
  (ANY "*"  404))
 
(defn -main [& args]
  (run-server {:port 8000} "/*" (servlet carcassonne)))


(defn startserv []
  "Used for debugging, start a server and save it in a variable"
  (def server (-main)))

(defn stopserv []
  "Stopps saved server"
  (stop server))