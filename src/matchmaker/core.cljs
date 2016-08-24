(ns matchmaker.core
    (:require [reagent.core :as reagent :refer [atom]]))

(defn parse-players [txt]
  [])

(defn parse-attendance [txt]
  [])

(defn can-play [players attendance]
  [])

(defn group-teams [players]
  [])

;; -------------------------
;; Views

(defn editor [value]
  [:textarea {:value @value
              :on-change #(reset! value (-> % .-target .-value))}])

(def players-txt (atom ""))
(def attendance-txt (atom ""))

(defn root []
  [:div
   [editor players-txt]
   [editor attendance-txt]
   (let [players (parse-players players-txt)
         attendance (parse-attendance attendance-txt)
         teams (group-teams (can-play players attendance))]
     [:p "TODO: Everything. " (count @players-txt) " - " (count @attendance-txt)])])

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [root] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
