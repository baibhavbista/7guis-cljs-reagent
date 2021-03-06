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
    :component counter}
   {:title "Temperature Converter"
    :component tempr-converter}
   {:title "Flight Booker"
    :component flight-booker}
   {:title "Timer"
    :component timer}
   {:title "CRUD"
    :component crud}
   {:title "Circle Drawer"
    :description-hiccup
    [:p 
     "Click on the canvas to create a circle" [:br]
     "Right click on a circle to get option to resize it. Click anywhere on the canvas outside the resize window after you are done resizing."]
    :component circle-drawer}
   {:title "Cells"
    :description-hiccup
    [:p
     "Formulas have to be prefixed with a '='" [:br]
     "operations available: add, sub, div, mul, mod, sum, prod" [:br]
     "Cells representation examples:  A0, Z99" [:br]
     "A range example  C3:C10" [:br]
     "Usage examples: (Note no spaces!!)" [:br]
     "=add(3,4)" [:br]
     "=sub(4,D3)" [:br]
     "=div(3,2)" [:br]
     "=mul(D1,D3)" [:br]
     "=mod(D3,D1)" [:br]
     "=sum(3,4,5)" [:br]
     "=sum(D1,D2,D5)" [:br]
     "=sum(D1:D3)" [:br]
     "=prod(3,4,5)" [:br]
     "=prod(D1,D2,D5)" [:br]
     "=prod(D1:D3)" [:br]]
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
    (for [{:keys [title description-hiccup component]} tasks]
      ^{:key title}
      [container title description-hiccup [component]])]])

(rd/render [app]
           (. js/document (getElementById "app")))
