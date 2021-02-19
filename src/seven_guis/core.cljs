(ns seven-guis.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.dom :as rd]
              [seven-guis.tasks.counter :refer [counter]]
              [seven-guis.tasks.tempr-converter :refer [tempr-converter]]
              [seven-guis.tasks.flight-booker :refer [flight-booker]]
              [seven-guis.tasks.timer :refer [timer]]
              [seven-guis.tasks.crud :refer [crud]]))

(enable-console-print!)

(println "This text is printed from src/seven-guis/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))

(defn app []
  [:div
   [counter]
   [tempr-converter]
   [flight-booker]
   [timer]
   [crud]])

(rd/render [app]
           (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
