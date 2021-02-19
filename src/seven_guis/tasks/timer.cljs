(ns seven-guis.tasks.timer
  (:require [reagent.core :as r]
            [cljs.core.async :as async :refer [chan go-loop timeout <! >!]]))

;;; NOTE: cljs.core.async is probably over-the-top for this problem, but I had just learned about it and wanted to practice
;;; A simpler solution would have been if js/setInterval was used for the "tick"

;; Initial state of timer
;; values of both :elapsed and :duration are times and are in seconds
(def initial-state {:elapsed  0
                    :duration 11})

;; function that returns a channel prepended with "<", as
;; per suggestion by Eric Normand
(defn <tick-channel 
  "Returns a somewhat-drift-correcting channel that ticks every `tick-interval-ms` milliseconds"
  [tick-interval-ms]
  (let [tick-chan (chan)
        start-time (js/Date.now)]
    (go-loop [i 1]
      (let [delay (- (+ start-time (* i tick-interval-ms))
                     (js/Date.now))] ; delay as a measure of reducing drift
        (<! (timeout delay))
        (>! tick-chan i))
      (recur (inc i)))
    tick-chan))

(defn set-duration!
  "Given an atom that is of the form {:elapsed ... :duration ...}, sets value for :duration to `duration`.
   In case new `duration` is less than (:elapsed timer-state), also modifies :elapsed "
  [timer-state duration]
  (if (> (:elapsed @timer-state) duration)
    ;; if elapsed time > new duration, timer is made full
    ;;    set both :elapsed-ms and :duration-ms to new duration
    (swap! timer-state merge {:duration duration
                              :elapsed  duration})
    ;; else if elapsed time < new duration time, only update :duration-ms and continue as normal
    (swap! timer-state assoc  :duration duration)))

(defn reset-timer! 
  "Resets the timer whose state is stored in `timer-state` i.e. sets elapsed time to 0 "
  [timer-state]
  (swap! timer-state assoc :elapsed 0))

;; used type 2 form of reagent components to do some setting up of states, go blocks and tick channels 
(defn timer
  []
  (let [timer-state      (r/atom initial-state)
        tick-interval-ms 50
        tick-interval-s  (/ tick-interval-ms 1000)
        tick-channel     (<tick-channel tick-interval-ms)]
    ;; a go block that increments :elapsed of `timer-state` at every tick of `tick-channel` 
    (go-loop []
      (<! tick-channel)
      (swap! timer-state
             (fn [{:keys [elapsed duration] :as state}]
               (assoc state :elapsed (min duration
                                          (+ elapsed tick-interval-s)))))
      (recur))
    (fn []
      (let [{:keys [elapsed duration]} @timer-state
            display-elapsed  (str (.toFixed elapsed 1) "s")
            display-duration (str (.toFixed duration 0) "s")]
        [:div.form
         [:label
          "Elapsed Time:"
          [:progress
           {:value elapsed
            :max   duration}]
          [:output display-elapsed]]
         [:br]
         [:label "Duration"
          [:input
           {:type :range
            :value duration
            :on-change (fn [e] (set-duration! timer-state
                                              (js/parseFloat (.. e -target -value))))
            :step 1
            :min 0
            :max 100}]
          [:output display-duration]]
         [:br]
         [:button
          {:on-click (fn [_e] (reset-timer! timer-state))}
          "Reset"]]))))