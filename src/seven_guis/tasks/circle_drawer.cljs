(ns seven-guis.tasks.circle-drawer
  (:require [reagent.core :as r]
            [seven-guis.utils :refer [input-event->value]]
            [reagent-contextmenu.menu :as menu]))

;;; P.S. added a library `reagent-contextmenu` for the context menu as it looks pretty good
;;; TODO-LATER: replace it with own context-menu component

;;; REPRESENTATION OF CIRCLE 
;;; A map with keys :x :y :r for x-coordinate, y-coordinate and radius respectively
;;; {:x x :y y :r r}

(defn- create-circle-map 
  "Returns a circle centered at (`x`, `y`) and with radius `r`"
  [x y r]
  {:x x :y y :r r})

(def DEFAULT_RADIUS 30)

;; no circles in init-working-state
;; demo initialized at the end of the file, not done here so that can use undo and redo for those as well
(def init-working-state [])

;;; TYPES OF STATES
;;; working-state: Current working state of data, it is a vector of circles displayed on the screen. 
;;;                init-working-state is our initial working state 
;;; app-state:     Complete state of app. 
;;;                app-state contains 
;;;                1. working-state, 
;;;                2. history: list of older working-states. 
;;;                       Note that it is a LIST: new states when checkpointed are inserted to the from of this list
;;;                       rationale for list: we will be mostly dealing with elements at the front of the list for 
;;;                           adding new states, undoing, redoing and deleting states after change when undo
;;;                       (list init-working-state) is our initial history
;;;                3. current-index-in-history : where we are in history
;;;                       normally 0 (pointing to front of list)
;;;                       when undo is done, current-index-in-history is incremented (i.e. going to older states)
;;;                       when redo is done, current-index-in-history is decremented (i.e. going to newer states)
;;;                4. idx-of-circle-to-resize: If circle is selected for resizing (by right clicking and selecting option "adjust diameter"),
;;;                       this contains the circle's index in working-state
;;;                       also used for display of resize-dialog, which is shown if this value is non-null
;;;
;;; NOTE: this is an important distinction. When you see working state below, it only means a vector of circles. 
;;; However, when you see mention to app state, it refers to the entire app state, which includes the working state as well 

(defonce app-state
  (let [init-history (list init-working-state)]
    (r/atom {:working-state            init-working-state
             :history                  init-history
             :current-index-in-history 0
             :idx-of-circle-to-resize  nil})))

;;; APP STATE UPDATE FUNCTIONS
;;; one function `update-working-state!` which does the side-effecty bits
;;; two pure functions `next-app-state-without-checkpoint` and `next-app-state-with-checkpoint` that take the old app state
;;;     along with function that maps from old to new working state, and returns the new app state, with or without checkpointing
;;;     Right now, without checkpoint version used for changing radius of circle within dialog box (before closing the dialog)
;;;                with checkpoint version used for creating circles and for final value of new radius after resize-dialog closed  

(defn update-working-state!
  "Updates working state of atom `app-state` by applying function `f`.
   `f` takes old working state and returns new working state
   NOTE: does not checkpoint and there should only be used for changes that don't have to be stored in undo/redo history.
   For significant changes (wrt undo/redo history), use the function update-working-state-and-checkpoint! "
  [app-state f]
  (swap! app-state update :working-state f))

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
  (let [{:keys [history current-index-in-history working-state]} old-app-state
        new-working-state (next-working-state-f working-state)
        new-current-index-in-history 0
        new-history (->> history
                         ;; if in a state where possibility of redo, drop all those states we can reach by redo, 
                         ;; they should not be able to be accessed after editing
                         (drop current-index-in-history)
                         ;; add new working state to the front of history list 
                         (cons new-working-state))]
    (assoc old-app-state
           :working-state new-working-state
           :history       new-history
           :current-index-in-history new-current-index-in-history)))


;;; UNDO FUNCTIONALITY

(defn can-undo?
  "Returns true if we have a state in history we can undo to"
  [current-index-in-history history]
  (let [new-index-after-undo (inc current-index-in-history)]
    ;; we can undo if there is a state in `new-index-after-undo` index of state
    ;; i.e. if `new-index-after-undo` < num of states in history
    (< new-index-after-undo (count history))))

(defn- next-app-state-after-undo
  "Takes the `old-app-state` and returns the app-state we get after applying undo"
  [old-app-state]
  (let [{:keys [current-index-in-history history]} old-app-state
        new-index         (inc current-index-in-history)
        new-working-state (nth history new-index)]
    (assoc old-app-state
           :current-index-in-history new-index
           :working-state new-working-state)))

(defn undo!
  "Undoes last significant change which may be either creation or resizing"
  [app-state]
  (swap! app-state next-app-state-after-undo))


;;; REDO FUNCTIONALITY 

(defn can-redo?
  [current-index-in-history]
  (let [new-index-after-redo (dec current-index-in-history)]
    (>= new-index-after-redo 0)))

(defn- next-app-state-after-redo
  "Takes the `old-app-state` and returns the app-state we get after applying redo"
  [old-app-state]
  (let [{:keys [current-index-in-history history]} old-app-state
        new-index         (dec current-index-in-history)
        new-working-state (nth history new-index)]
    (assoc old-app-state :current-index-in-history new-index
           :working-state new-working-state)))

(defn redo!
  "Redoes latest significant change which was undone"
  [app-state]
  (swap! app-state next-app-state-after-redo))


;;; EDIT FUNCTIONS

(defn create-circle! 
  "Create a circle centered at (`x`, `y`) and having radius `r` (default value used if not supplied)"
  ([app-state x y] (create-circle! app-state x y DEFAULT_RADIUS))
  ([app-state x y r]
   (let [circle-map (create-circle-map x y r)]
     (swap! app-state
            next-app-state-with-checkpoint
            #(conj % circle-map)))))

(defn update-radius-circle!
  "Update the circle at index `idx` of working-state to have radius `r`. 
   DOES NOT CHECKPOINT and so, can be used for `micro`-updates from within resize-dialog."
  [app-state idx r]
  (swap! app-state
         next-app-state-without-checkpoint
         (fn [old-working-state-vecmap]
           (assoc-in old-working-state-vecmap [idx :r] r))))


;;; RESIZE DIALOG BOX OPEN CLOSE

(defn set-and-show-resize-dialog!
     "Set state corresponding to resize dialog and show the dialog"
     [app-state circle-idx]
     (swap! app-state assoc :idx-of-circle-to-resize circle-idx))

(defn save-and-close-resize-dialog!
  "Close the resize dialog. If changes have been made, checkpoint as a significant event in history"
  [app-state]
  (swap! app-state assoc :idx-of-circle-to-resize nil)
  (let [working-state (:working-state @app-state)
        last-saved-state (-> @app-state :history first)]
    ;; when radius has been changed from within resize-dialog i.e. working state and last saved state are different
    ;;     add the current working-state to history 
    (when (not= working-state last-saved-state)
      ;; pass `identity` as `next-working-state-f` as we already have required new radius in our working-state
      (swap! app-state next-app-state-with-checkpoint identity))))


;;; CLICK HANDLERS

(defn handle-left-click-on-canvas
  "Extracts coordinates from click event `e` and creates a circle centered at extracted coordinate"
  [app-state e]
  (let [bounding-client-rect (-> e .-target .getBoundingClientRect)
        client-x (-> e .-clientX)
        client-y (-> e .-clientY)
        x-coord (- client-x (.-x bounding-client-rect))
        y-coord (- client-y (.-y bounding-client-rect))]
    #_(println "Left clicked on" x-coord "," y-coord)
    (create-circle! app-state x-coord y-coord)))


(defn handle-right-click-on-circle
  "Triggers custom context menu"
  [app-state evt circle-idx]
  #_(println "Right clicked on circle with index" circle-idx)
  (menu/context!
   evt
   ;; the context menu has a single option to adjust diameter of the circle 
   [[[:span.cmd "Adjust diameter..."]
     #(set-and-show-resize-dialog! app-state circle-idx)]]))



;;; REAGENT COMPONENTS

(defn resize-dialog 
  "Reagent component for the dialog box for resizing diameter (radius) of selected circle. 
   Activated when right clicked on the circle and clicked on option to resize diameter."
  []
  (let [circle-idx (get-in @app-state [:idx-of-circle-to-resize])
        circle-map (get-in @app-state [:working-state circle-idx])
        float->display-string #(str (.toFixed % 2)) 
        x-coord-display-string (float->display-string (get circle-map :x))
        y-coord-display-string (float->display-string (get circle-map :y))]
    [:div
     [:div.circle-drawer--overlay {:on-click #(save-and-close-resize-dialog! app-state)}]
     [:div.circle-drawer--dialog.flex-column
      [:label
       (str "Adjust diameter of circle at (" x-coord-display-string ", " y-coord-display-string "):")]
      [:input
       {:type :range
        :value (:r circle-map)
        :on-change (fn [e]
                     (let [new-r (js/parseFloat (input-event->value e))]
                       (update-radius-circle! app-state circle-idx new-r)))
        :step 1
        :min 1
        :max 200}]]]))

(defn circle-svg
  "Reagent component for a single circle inside an :svg.
   Inputs:`circle-map` having keys :x :y :r,
          `is-selected-for-resizing` a boolean that is true when circle ahs been selected for resizing
          and `right-click-handler`"
  [circle-map is-selected-for-resizing right-click-handler]
  [:circle
   {:class (when is-selected-for-resizing "selected")
    :cx (:x circle-map)
    :cy (:y circle-map)
    :r  (:r circle-map)
    :fill "white"
    :stroke "black"
    :on-click (fn [e] (.preventDefault e) (.stopPropagation e))
    :on-context-menu right-click-handler}])

(defn circle-drawer
  "Reagent component for task 6: Circle Drawer"
  []
  [:div.flex-column.circle-drawer--wrapper
   [:div.flex-row.circle-drawer--buttons
    [:button
     {:on-click (fn [_e] (undo! app-state))
      :disabled (not (can-undo? (:current-index-in-history @app-state)
                                (:history @app-state)))}
     "Undo"]
    [:button
     {:on-click (fn [_e] (redo! app-state))
      :disabled (not (can-redo? (:current-index-in-history @app-state)))}
     "Redo"]]
   [:div.flex-row.circle-drawer--canvas
    [menu/context-menu]
    [:svg.circle-drawer--svg
     {:on-click #(handle-left-click-on-canvas app-state %)}
     (for [[idx circle-map] (map-indexed vector (get @app-state :working-state))
           :let [is-selected-for-resizing (= idx (get @app-state :idx-of-circle-to-resize))]]
       ^{:key idx} [circle-svg
                    circle-map
                    is-selected-for-resizing
                    (fn [e] (handle-right-click-on-circle app-state e idx))])]]
   (when (get @app-state :idx-of-circle-to-resize)
     [resize-dialog])])

;; load some initial circles
(defonce is-demo-loaded
  (do
    (create-circle! app-state 50 50)
    (create-circle! app-state 250 250 100)
    true))