(ns matchmaker.core
    (:require [reagent.core :as reagent :refer [atom]]
              [clojure.string :as string]))

(defonce players-tsv (atom ""))
(defonce attendance-tsv (atom ""))
(defonce shuffle-count (atom 0))
(defonce prefer-higher-ranks (atom false))

(def rank-scores
  {"NONE" 0
   "S1"   5
   "S2"   15
   "S3"   25
   "S4"   35
   "SE"   45
   "SEM"  55
   "GN1"  65
   "GN2"  75
   "GN3"  85
   "GNM"  95
   "MG1"  105
   "MG2"  115
   "MGE"  125
   "DMG"  135
   "LE"   145
   "LEM"  155
   "SMFC" 165
   "GE"   175})

(def help-messages {:incomplete (str "Please paste the players and their ranks"
                                     " from the spreadsheet as well as today's"
                                     " attendance into the boxes above. You can"
                                     " literally copy and paste them by"
                                     " highlighting them in your browser.")})

(defn columns [tsv]
  (let [rows (string/split tsv #"\n")
        cols (map #(string/split % #"[\t\s]") rows)]
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
  (loop [ps (cond->> players
                     true (sort-by #(rank-scores (:rank %)))
                     @prefer-higher-ranks (reverse))
         teams []]
        (if (< (count ps) 10)
          {:teams teams
           :remainder ps}
          (let [game (shuffle (take 10 ps))
                a (take 5 game)
                b (drop 5 game)]
            (recur (drop 10 ps) (conj teams [a b]))))))

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
  [:p (:name p) " " [:span.rank {:class (str "rank-" (:rank p))} (:rank p)]])

(defn player-list [players]
  [:ul.players (map (fn [p] [:li {:key (hash p)} (player p)]) players)])

(defn team [t]
  [:section.team-pair
   [:section.team-a
    (player-list (-> t (first)))]
   [:h4.vs "vs"]
   [:section.team-b
    (player-list (-> t (second)))]])

(defn team-list [teams]
  [:ul.teams (map (fn [t] [:li {:key (hash t)} (team t)]) teams)])

(defn results []
  @shuffle-count
  (let [result (make-teams)]
    [:section.results
     [:button.shuffle {:on-click #(swap! shuffle-count inc)} "Shuffle"]
     [:section.prefer-higher-ranks
      [:input#prefer-higher-ranks-checkbox
       {:type "checkbox"
        :checked @prefer-higher-ranks
        :on-change #(swap! prefer-higher-ranks not)}]
      [:label {:for "prefer-higher-ranks-checkbox"} "Try to build higher rank teams instead?"]]
     [:section.teams
      [:h3 "Teams"]
      (team-list (:teams result))]
     [:section.remainder
      [:h3 "Remainder"]
      (player-list (:remainder result))]]))

(defn root []
  [:section.root
   [:h1.title "TAW.net CS:GO team builder"]
   [:p.introduction
    "This tool helps the " [:a {:href "http://taw.net/"} "TAW.net"]
    " CS:GO division to build (fairly) balanced teams for our regular training games. It's written in "
    [:a {:href "http://clojurescript.org/"} "ClojureScript"] ", you can find the source on "
    [:a {:href "https://github.com/TAW-CSGO/matchmaker"} "GitHub"] ". "
    "To build the teams, you need to paste our members / ranks table and today's "
    "attendance list into the boxes below."]
   (tsv-editors)
   (if
     (or (empty? @players-tsv)
         (empty? @attendance-tsv)) [:p.incomplete (:incomplete help-messages)]
     (results))
   [:p.credit "Built with <3 by " [:a {:href "https://github.com/Olical"} "Olical"] "."]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [root] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
