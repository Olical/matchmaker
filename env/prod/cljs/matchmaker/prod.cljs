(ns matchmaker.prod
  (:require [matchmaker.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
