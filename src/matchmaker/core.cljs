(ns matchmaker.core
    (:require [reagent.core :as reagent :refer [atom]]
              [clojure.string :as string]))

(defonce players-tsv (atom ""))
(defonce attendance-tsv (atom ""))

(defn columns [tsv]
  (let [rows (string/split tsv #"\n")
        cols (map #(string/split % #"\t") rows)]
    cols))

(defn parse-players [tsv]
  (map (fn [c]
           {:name (nth c 0)
            :rank (nth c 3 "NONE")})
       (columns tsv)))

(defn parse-attendance [tsv]
  (let [pairs (map (fn [c] [(nth c 0) (= (nth c 1 nil) "attended")]) (columns tsv))]
    (into (sorted-map) pairs)))

(defn can-play [players attendance]
  (filter (fn [p] (attendance (:name p))) players))

(defn teams-by-skill [players]
  {:teams [{:name "foobar"
            :a {:name "foo"
                :players (take 5 players)}
            :b {:name "bar"
                :players (take 5 (drop 5 players))}}]
   :remainder players})

(defn make-teams []
  (let [players (parse-players @players-tsv)
        attendance (parse-attendance @attendance-tsv)]
    (-> players
        (can-play attendance)
        (teams-by-skill))))

;; -------------------------
;; Views

(defn editor [placeholder value]
  [:textarea.editor {:placeholder placeholder
                     :value @value
                     :on-change #(reset! value (-> % .-target .-value))}])

(defn tsv-editors []
  [:section.editors
   [editor "Players" players-tsv]
   [editor "Attendance" attendance-tsv]])

(defn player [p]
  [:p (:name p) " " [:span.rank (:rank p)]])

(defn player-list [players]
  [:ul.players (map (fn [p] [:li {:key (hash p)} (player p)]) players)])

(defn team [t]
  [:section.team-pair
   [:section.team-a 
    (player-list (-> t :a :players))]
   [:h4.vs "vs"]
   [:section.team-b
    (player-list (-> t :b :players))]])

(defn team-list [teams]
  [:ul.teams (map (fn [t] [:li {:key (hash t)} (team t)]) teams)])

(defn results []
  (let [result (make-teams)]
    [:section.results
     [:section.teams
      [:h3 "Teams"]
      (team-list (:teams result))]
     [:section.remainder
      [:h3 "Remainder"]
      (player-list (:remainder result))]]))

(defn root []
  [:section.root
   (tsv-editors)
   (results)])

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [root] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
