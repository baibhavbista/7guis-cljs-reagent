(ns seven-guis.tasks.timer
  (:require [reagent.core :as r]
            [cljs.core.async :as async :refer [chan go go-loop timeout <! >!]]))

(defn s->ms [s] (* s 1000))
(defn ms->s [ms] (/ ms 1000))

(def initial-state {:elapsed-ms  0
                    :duration-ms (s->ms 5)})

(defonce timer-state (r/atom initial-state))

;; TODO: core.async looks like diverges since swap! function takes 
;; some time. so, try with js/setInterval

(def tick-chan (chan))
(def tick-interval-ms 100)
(go (while true 
      (<! (timeout tick-interval-ms))
      (>! tick-chan "tick")))
(go
  (let [{:keys [elapsed-ms duration-ms]} @timer-state]
    (<! tick-chan)
    (swap! timer-state assoc :elapsed-ms
           (min duration-ms
                (+ elapsed-ms tick-interval-ms)))))

(defn fix-to-precision [digits]
  (fn [number]
    (-> number
        js/parseFloat
        (.toFixed digits))))

(defn display-time-s [time-s]
  (let [fix-to-1-digit-after-decimal (fix-to-precision 1)]
    (fix-to-1-digit-after-decimal time-s)))

(comment
  (display-time-s 3.423))

(defn set-duration! [timer-state duration-in-s]
  (let [duration-in-ms (s->ms duration-in-s)]
    (if (> (:elapsed-ms @timer-state) duration-in-ms)
      ;; if elapsed time > new duration, timer is made full
      ;;    set both :elapsed-ms and :duration-ms to new duration
      (swap! timer-state merge {:duration-ms duration-in-ms
                                :elapsed-ms duration-in-ms})
      ;; else if elapsed time < new duration time, only update :duration-ms and continue as normal
      (swap! timer-state assoc :duration-ms duration-in-ms))))

(defn reset-timer! [timer-state]
  (swap! timer-state assoc :elapsed-ms 0))

(defn timer []
  (let [{:keys [elapsed-ms duration-ms]} @timer-state
        elapsed-s (ms->s elapsed-ms)
        display-elapsed-s (display-time-s elapsed-s)
        duration-s (ms->s duration-ms)
        display-duration-s (display-time-s duration-s)]
    [:div.form
     [:label
      "Elapsed Time:"
      [:progress
       {:value elapsed-s
        :max   duration-s}]
      [:output display-elapsed-s]]
     [:br]
     [:label "Duration"
      [:input
       {:type :range
        :value duration-s
        :on-change (fn [e] (set-duration! timer-state
                                          (.. e -target -value)))
        :step 1
        :min 0
        :max 100}]
      [:output (js/parseInt display-duration-s)]]
     [:br]
     [:button
      {:on-click (fn [_e] (reset-timer! timer-state))}
      "Reset"]]))