(ns seven-guis.tasks.counter
  (:require [reagent.core :as r]))

(defn counter 
  []
  (let [click-counter (r/atom 0)]
    (fn []
      [:div
       [:span
        [:input.short-input 
         {:type "number"
          :value (str @click-counter)
          :read-only true}]
        [:button 
         {:on-click (fn [_e] (swap! click-counter inc))}
         "Count"]]])))
