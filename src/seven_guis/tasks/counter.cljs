(ns seven-guis.tasks.counter
  (:require [reagent.core :as r]))

(defn counter 
  []
  (let [click-counter (r/atom 0)]
    (fn []
      [:div
       [:div (str @click-counter)]
       [:button {:on-click 
                 (fn [_e] (swap! click-counter inc))}
        "Count"]])))
