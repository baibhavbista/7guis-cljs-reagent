(ns seven-guis.tasks.tempr-converter
  (:require [reagent.core :as r]
            [clojure.string :as string]))

(defn f->c
  "Convert `deg-f` (in Fahrenheit) to Celsius."
  [deg-f]
  (/ (- deg-f 32) 1.8))

(defn c->f
  "Convert `deg-c` (in Celsius) to Fahrenheit."
  [deg-c]
  (+ (* deg-c 1.8) 32))

(comment
  (= 32 (c->f (f->c 32)))
  (= 100 (f->c (c->f 100))))

;; initial value for state
(def init-celsius 0)

;; store temperatures as a map in a single ratom
;; meant to hold only temperature values (either number or nil if empty)
;; will have to change validator in case other types of data are to be stored
(defonce tempr-state (r/atom {:celsius    init-celsius
                              :fahrenheit (c->f init-celsius)}))
;; validator to ensure that temperatures in both units are either both numbers or both nil
(set-validator! tempr-state
                (fn [new-state]
                  (let [tempr-vals (vals new-state)
                        are-all-numbers? (every? number? tempr-vals)
                        are-all-nil? (every? nil? tempr-vals)]
                    (or are-all-numbers? are-all-nil?))))

(defn update-tempr!
  "Given `new-tempr` in `unit` (:celsius or :fahrenheit), updates both :celsius and :fahrenheit values in the ratom `tempr-state`.
   If `new-tempr` is nil, empties both fields (both values are set nil)"
  [tempr-state unit new-tempr]
  (if new-tempr
    ;; valid temperature
    (case unit
      :celsius    (reset! tempr-state {:celsius new-tempr
                                       :fahrenheit (c->f new-tempr)})
      :fahrenheit (reset! tempr-state {:fahrenheit new-tempr
                                       :celsius (f->c new-tempr)}))
    ;; invalid temperature, empty the fields
    (reset! tempr-state {})))

(comment
  ; running these should change both celsius and fahrenheit values in the browser 
  (update-tempr! tempr-state :celsius 100)
  (update-tempr! tempr-state :fahrenheit 100))


(defn event->float
  "Extracts value string from event `e` (in js, event.target.value) and converts it to float."
  [e]
  (let [val-str (-> e .-target .-value)]
    (when-not (string/blank? val-str)
      (js/parseFloat val-str))))

(defn tempr-input
  "Reagent component for a single input element (of type number) for temperatures of given `unit`."
  [unit]
  [:input {:type "number"
           :step 0.1
           :value (get @tempr-state unit "")
           :on-change #(let [new-tempr (event->float %)]
                         (update-tempr! tempr-state unit new-tempr))}])

(defn tempr-converter []
  [:div
   [:span
    [tempr-input :celsius]
    [:label " Celsius"]]
   " = "
   [:span
    [tempr-input :fahrenheit]
    [:label " Fahrenheit"]]])