(ns seven-guis.core
    (:require [reagent.dom :as rd]
              [seven-guis.container :refer [container]]
              [seven-guis.tasks.counter :refer [counter]]
              [seven-guis.tasks.tempr-converter :refer [tempr-converter]]
              [seven-guis.tasks.flight-booker :refer [flight-booker]]
              [seven-guis.tasks.timer :refer [timer]]
              [seven-guis.tasks.crud :refer [crud]]
              [seven-guis.tasks.circle-drawer :refer [circle-drawer]]
              [seven-guis.tasks.cells :refer [cells]]))

(enable-console-print!)

(def tasks
  [{:title "Counter"
    :description "sth"
    :component counter}
   {:title "Temperature Converter"
    :description "sth"
    :component tempr-converter}
   {:title "Flight Booker"
    :description "sth"
    :component flight-booker}
   {:title "Timer"
    :description "sth"
    :component timer}
   {:title "CRUD"
    :description "sth"
    :component crud}
   {:title "Circle Drawer"
    :description "sth"
    :component circle-drawer}
   {:title "Cells"
    :description "sth"
    :component cells}])


(defn app []
  [:div
   [:div
    [:h1 "7GUIs solutions in Clojurescript + Reagent"]
    [:p 
     [:a
      {:href "https://eugenkiss.github.io/7guis/"}
      "7GUIs"]
     " is a GUI programming benchmark that defines seven tasks that represent typical challenges in GUI programming."]
    [:p 
     "Source code:" 
     [:a
      {:href "https://github.com/baibhavbista/7guis-cljs-reagent"}
      "Github"]]]
   [:div
    (for [{:keys [title description component]} tasks]
      ^{:key title}
      [container title description [component]])]])

(rd/render [app]
           (. js/document (getElementById "app")))
