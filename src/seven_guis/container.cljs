(ns seven-guis.container)

(defn container 
  [title description children]
  [:section {:class "container"}
   [:h2 title]
   [:p description]
   [:div.centered-wrapper
    children]])