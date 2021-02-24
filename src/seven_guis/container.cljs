(ns seven-guis.container
  (:require [clojure.string :as string]))

(defn container [title description children]
  (let [dash-separated-title (string/lower-case (string/replace title #" " "-"))]
    [:section {:class (str "container " dash-separated-title)}
     [:h2 title]
     [:p description]
     [:div.centered-wrapper 
      children]]))