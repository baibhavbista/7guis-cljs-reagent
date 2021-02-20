(ns seven-guis.tasks.circle-drawer
  (:require [reagent.core :as r]
            [seven-guis.utils :refer [vecmap-assoc-pred input-event->value]]
            [reagent-contextmenu.menu :as menu]))

;;; P.S. added a library `reagent-contextmenu` for the context menu as it looks pretty good
;;; TODO-LATER: replace it with own context-menu component

;;; REPRESENTATION OF CIRCLE 
;;; A map with keys :x :y :r for x-coordinate, y-coordinate and radius respectively
;;; {:x x :y y :r r}

(def DEFAULT_RADIUS 30)

(def init-working-state [{:x 100 :y 100 :r 20}])

;;; TYPES OF STATES
;;; working-state: Current working state of data, it is a vector of circles displayed on the screen. 
;;;                init-working-state is our initial working state 
;;; app-state:     Complete state of app. 
;;;                app-state contains 
;;;                1. working-state, 
;;;                2. history: list of older working-states. 
;;;                            Note that it is a list: new states when checkpointed are inserted to the from of this list
;;;                            (list init-working-state) is our initial history
;;;                3. current-index : where we are in history
;;;                                   normally 0 (pointing to front of list)
;;;                                   when undo is done, current-index is incremented (i.e. going to older states)
;;;                                   when redo is done, current-index is decremented (i.e. going to newer states)
;;;
;;; NOTE: this is an important distinction. When you see working state below, it only means a vector of circles. 
;;; However, when you see mention to app state, it refers to the entire app state, which includes the working state as well 

(defonce app-state
  (let [init-history (list init-working-state)]
    (r/atom {:working-state init-working-state
             :history       init-history
             :current-index 0
             :resize-dialog {:x 0
                             :y 0
                             :r 0
                             :display nil}})))

(defn update-working-state!
  "Updates working state of atom `app-state` by applying function `f`.
   `f` takes old working state and returns new working state
   NOTE: does not checkpoint and there should only be used for changes that don't have to be stored in undo/redo history.
   For significant changes (wrt undo/redo history), use the function update-working-state-and-checkpoint! "
  [app-state f]
  (swap! app-state update :working-state f))

(drop 0 '(1 2 3))

(defn next-app-state-without-checkpoint
  "Returns new app-state after applying `next-working-state-f` to working state i.e. (:working-state old-app-state)
   NOTE: this function does not checkpoint and therefore should only be used for changes that don't have to be stored in undo/redo history.
         For significant changes (wrt undo/redo history), use the function `next-app-state-with-checkpoint`"
  [old-app-state next-working-state-f]
  (update old-app-state :working-state next-working-state-f))

(defn next-app-state-with-checkpoint
  "Returns new app-state after applying `next-working-state-f` to working state i.e. (:working-state old-app-state)
   This function also checkpoints the new working-state i.e. adds the new working-state to :history list"
  [old-app-state next-working-state-f]
  (let [{:keys [history current-index working-state]} @app-state
        new-working-state (next-working-state-f working-state)
        new-current-index 0
        new-history (->> history
                         ;; if in a state where possibility of redo, drop all those states we can reach by redo, 
                         ;; they should not be able to be accessed after editing
                         (drop current-index)
                         ;; add new working state to the front of history list 
                         (cons new-working-state))]
    (assoc old-app-state :working-state new-working-state
           :history       new-history
           :current-index new-current-index)))

(defn- next-app-state-after-undo
  [old-app-state]
  (let [{:keys [current-index history]} old-app-state
        new-index         (inc current-index)
        new-working-state (nth history new-index)]
    (assoc old-app-state :current-index new-index
           :working-state new-working-state)))

(defn undo!
  [app-state]
  (swap! app-state next-app-state-after-undo))

(defn can-undo?
  [current-index history]
  (let [new-index-after-undo (inc current-index)]
    ;; we can undo if there is a state in `new-index-after-undo` index of state
    ;; i.e. if `new-index-after-undo` < num of states in history
    (< new-index-after-undo (count history))))

(defn- next-app-state-after-redo
  [old-app-state]
  (let [{:keys [current-index history]} old-app-state
        new-index         (dec current-index)
        new-working-state (nth history new-index)]
    (assoc old-app-state :current-index new-index
           :working-state new-working-state)))

(defn redo!
  [app-state]
  (swap! app-state next-app-state-after-redo))

(defn can-redo?
  [current-index]
  (let [new-index-after-redo (dec current-index)]
    (>= new-index-after-redo 0)))

(defn update-radius-circle!
  "Update the circle centered at (`x`, `y`) to have radius `r`"
  [app-state x y r]
  (let [is-required-circle? (fn [circle-map]
                              (and (= x (:x circle-map))
                                   (= y (:y circle-map))))]
    (swap! app-state
           next-app-state-without-checkpoint
           ;; function that takes a list of circles(working-state)
           ;; and updates the circle with center (`x`, `y`) to have radius `r`  
           (fn [old-working-state-vecmap]
             (vecmap-assoc-pred old-working-state-vecmap
                                is-required-circle?
                                :r
                                r)))))

(defn create-circle! [app-state x y]
  (let [circle-map {:x x :y y :r DEFAULT_RADIUS}]
    (swap! app-state
           next-app-state-with-checkpoint
           #(conj % circle-map))))

(defn handle-left-click-on-canvas [app-state e]
  (let [bounding-client-rect (-> e .-target .getBoundingClientRect)
        client-x (-> e .-clientX)
        client-y (-> e .-clientY)
        x-coord (- client-x (.-x bounding-client-rect))
        y-coord (- client-y (.-y bounding-client-rect))]
    #_(println "Left clicked on" x-coord "," y-coord)
    (create-circle! app-state x-coord y-coord)))

(defn set-and-show-resize-dialog! [app-state x y r]
  (swap! app-state assoc :resize-dialog {:x x
                                         :y y
                                         :r r
                                         :display "block"}))

(defn save-and-close-resize-dialog! [app-state]
  (swap! app-state assoc :resize-dialog nil)
  (let [working-state (:working-state @app-state)
        last-saved-state (-> @app-state :history first)]
    ;; when radius has been changed from within resize-dialog i.e. working state and last saved state are different
    ;;     add the current working-state to history 
    (when (not= working-state last-saved-state)
      ;; pass `identity` as `next-working-state-f` as we already have required new radius in our working-state
      (swap! app-state next-app-state-with-checkpoint identity))))


(defn handle-right-click-on-circle [app-state e]
  ;; (.preventDefault e)
  ;; (.stopPropagation e)
  (let [circle (-> e .-target)
        cx-val (-> circle .-cx .-baseVal .-value)
        cy-val (-> circle .-cy .-baseVal .-value)
        r-val  (-> circle .-r  .-baseVal .-value)]
    #_(println "Right clicked on circle of radius" r-val "at (" cx-val "," cy-val ")")
    (menu/context!
     e
     [[[:span.cmd "Adjust diameter..."] 
       #(set-and-show-resize-dialog! app-state cx-val cy-val r-val)]])))

(defn resize-dialog []
  (let [state (r/cursor app-state [:resize-dialog])]
    (when (:display @state)
      [:div
       [:div.circle-drawer--overlay {:on-click #(save-and-close-resize-dialog! app-state)}]
       [:div.circle-drawer--dialog.column
        [:label
         (str "Adjust diameter of circle at (" (:x @state) ", " (:y @state) ").")]
        [:input
         {:type :range
          :value (:r @state)
          :on-change (fn [e]
                       (let [new-r (js/parseFloat (input-event->value e))]
                         (swap! state assoc :r new-r)
                         (update-radius-circle! app-state (:x @state) (:y @state) new-r)))
          :step 1
          :min 1
          :max 200}]]])))

(defn circle-svg [{:keys [x y r]} right-click-handler]
  [:circle {:cx x
            :cy y
            :r r
            :fill "white"
            :stroke "black"
            :on-click (fn [e] (.preventDefault e) (.stopPropagation e))
            :on-context-menu right-click-handler}])

(defn circle-drawer []
  [:div.column.circle-drawer--wrapper
   [resize-dialog]
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
    [menu/context-menu]
    [:svg {:on-click #(handle-left-click-on-canvas app-state %)}
     (for [[id circle-map] (map-indexed vector (get @app-state :working-state))]
       ^{:key id} [circle-svg 
                   circle-map
                   (fn [e] (handle-right-click-on-circle app-state e))])]
    #_(into [:svg {:on-click #(handle-left-click-on-canvas app-state %)}]
            (map circle-svg (get @app-state :working-state)))]])