(ns matchmaker.core
    (:require [reagent.core :as reagent :refer [atom]]
              [clojure.string :refer [split]]))

(def players-tsv (atom ""))
(def attendance-tsv (atom ""))

(defn columns [tsv]
  (let [rows (split tsv #"\n")
        cols (map #(split % #"\t") rows)]
    cols))

(defn parse-players [tsv]
  (map (fn [c]
           {:name (nth c 0)
            :rank (nth c 3 nil)})
       (columns tsv)))

(defn parse-attendance [tsv]
  (let [pairs (map (fn [c] [(nth c 0) (= (nth c 1 nil) "attended")]) (columns tsv))]
    (into (sorted-map) pairs)))

(defn can-play [players attendance]
  (filter (fn [p] (attendance (:name p))) players))

(defn group-teams [players]
  [])

;; -------------------------
;; Views

(defn editor [placeholder value]
  [:textarea {:placeholder placeholder
              :value @value
              :on-change #(reset! value (-> % .-target .-value))}])

(defn root []
  [:div
   [editor "Players" players-tsv]
   [editor "Attendance" attendance-tsv]
   (let [players (parse-players players-tsv)
         attendance (parse-attendance attendance-tsv)
         teams (group-teams (can-play players attendance))]
     [:p "TODO: Everything. " (count @players-tsv) " - " (count @attendance-tsv)])])

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [root] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
