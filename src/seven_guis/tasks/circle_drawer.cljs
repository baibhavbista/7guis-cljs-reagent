(ns seven-guis.tasks.circle-drawer
  (:require [reagent.core :as r]))

(def DEFAULT_RADIUS 30)

(def init-state [])

(defonce app-state
  (let [init-history (list init-state)]
    (r/atom {:history       init-history
             :current-index 0
             :working-state init-state})))

(defn vecmap-assoc-pred
  "Matches the first map in the vector of maps `vm` that satisfies given predicate `pred` and maps `key` to `val` in the matched map"
  [vm pred key val]
  (let [i (some (fn [[i m]] (when (pred m) i))
                (map-indexed vector vm))]
    (update vm i #(assoc % key val))))

(defn update-working-state!
  "Updates working state of atom `app-state` by applying function `f`.
   `f` takes old working state and returns new working state
   NOTE: does not checkpoint and there should only be used for changes that don't have to be stored in undo/redo history.
   For significant changes (wrt undo/redo history), use the function update-working-state-and-checkpoint! "
  [app-state f]
  (swap! app-state update :working-state f))

(drop 0 '(1 2 3))

(defn update-working-state-and-checkpoint!
  [app-state f]
  (let [{:keys [history current-index working-state]} @app-state
        new-working-state (f working-state)
        new-current-index 0
        new-history (->> history
                         ;; if in states where possibility of redo, drop all those states we can reach by redo, since we should not be able to access
                         ;; such states after editing
                         (drop current-index)
                         ;; add new-working state to the front of history list 
                         (cons new-working-state))]
    (swap! app-state 
           #(assoc % :working-state new-working-state
                     :history       new-history
                     :current-index new-current-index))))

(defn undo!
  [app-state]
  (swap! app-state (fn [{:keys [current-index history] :as old-state}]
                     (let [new-index         (inc current-index)
                           new-working-state (nth history new-index)]
                       (assoc old-state :current-index new-index
                                        :working-state new-working-state)))))

(defn can-undo?
  [current-index history]
  (let [new-index-after-undo (inc current-index)]
    ;; we can undo if there is a state in `new-index-after-undo` index of state
    ;; i.e. if `new-index-after-undo` < num of states in history
    (< new-index-after-undo (count history))))

(defn redo!
  [app-state]
  (swap! app-state (fn [{:keys [current-index history] :as old-state}]
                     (let [new-index         (dec current-index)
                           new-working-state (nth history new-index)]
                       (assoc old-state :current-index new-index
                                        :working-state new-working-state)))))

(defn can-redo?
  [current-index]
  (let [new-index-after-redo (dec current-index)]
    (>= new-index-after-redo 0)))

(assoc {:a 1} :a 2 :b 3 :d 21)

(defn update-radius-circle! 
  "Update the circle centered at (`x`, `y`) to have radius `r`"
  [app-state x y r]
  (let [is-required-circle? (fn [circle-map]
                              (and (= x (:x circle-map))
                                   (= y (:y circle-map))))]
    (update-working-state! app-state
                           (fn [old-working-state-vecmap]
                             ;; check if can be done using map
                             (vecmap-assoc-pred old-working-state-vecmap
                                                is-required-circle?
                                                :r
                                                r)))))

(defn create-circle! [app-state x y]
  (let [circle-map {:x x :y y :r DEFAULT_RADIUS}]
    (update-working-state-and-checkpoint! app-state
                                          #(conj % circle-map))))

(defn handle-left-click-on-canvas [app-state e]
  (let [bounding-client-rect (-> e .-target .getBoundingClientRect)
        client-x (-> e .-clientX)
        client-y (-> e .-clientY)
        x-coord (- client-x (.-x bounding-client-rect))
        y-coord (- client-y (.-y bounding-client-rect))]
    (println "Left clicked on" x-coord "," y-coord)
    (create-circle! app-state x-coord y-coord)))

(defn handle-right-click-on-circle [e]
  (.preventDefault e)
  (.stopPropagation e)
  (let [circle (-> e .-target)
        cx-val (-> circle .-cx .-baseVal .-value)
        cy-val (-> circle .-cy .-baseVal .-value)
        r-val  (-> circle .-r  .-baseVal .-value)]
    (println "Right clicked on circle of radius" r-val "at (" cx-val "," cy-val ")")))

(defn resize-frame []
  [:div
   [:div.overlay {:on-click #(str "End resize here" %)}
    [:div.resizer]]])

(defn circle-svg [{:keys [x y r]}]
  [:circle {:cx x
            :cy y
            :r r
            :fill "white"
            :stroke "black"
            :on-click (fn [e] (.preventDefault e) (.stopPropagation e))
            :on-context-menu handle-right-click-on-circle}])

(defn circle-drawer []
  [:div.column.circle-drawer--wrapper
   [:div.row.circle-drawer--buttons
    [:button 
     {:on-click (fn [_e] (undo! app-state))
      :disabled (not (can-undo? (:current-index @app-state)
                                (:history @app-state)))}
     "Undo"]
    [:button 
     {:on-click (fn [_e] (redo! app-state))
      :disabled (not (can-redo? (:current-index @app-state)))}
     "Redo"]]
   [:div.row.circle-drawer--canvas
    (into [:svg {:on-click #(handle-left-click-on-canvas app-state %)}]
          (map circle-svg (get @app-state :working-state)))]])