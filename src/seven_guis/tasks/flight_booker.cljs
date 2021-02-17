(ns seven-guis.tasks.flight-booker
  (:require [reagent.core :as r]
            [clojure.string :as string]))

(defn- date->string 
  "Takes a JS Date object `date` and returns a string of format `dd.mm.yyyy`"
  [date]
  (let [pad-zero #(.padStart (.toString %) 2 "0")
        y (.getFullYear date)
        m (-> (.getMonth date) inc pad-zero)
        d (pad-zero (.getDate date))]
    (str d "." m "." y)))

(comment
  (date->string (js/Date.)))

;; do not handle date or date-string oneself in real code
;; in real projects, use sth like moment.js
;; In real projects, you would also use sth like an input with type=date,
;; solving half of the problems we're dealing with here
(defn string->date
  "Converts `date-string` (valid format `dd.mm.yyyy`)
   returns JS Date object if valid string 
           & nil if invalid string
   Therefore, can also be used to check if `date-string` is valid"
  [date-string]
  (try
    (let [dot-separated-date-regex #"^([0-9]{2})\.([0-9]{2})\.([0-9]{4})$"
          possibly-invalid-date (->> date-string
                                     (re-matches dot-separated-date-regex) ; if matched, outputs ["dd.mm.yyyy" dd mm yyyy]
                                     rest                                  ; outputs [dd mm yyyy]
                                     reverse                               ; outputs [yyyy mm dd]
                                     (clojure.string/join "-")                     ; outputs "yyyy-mm-dd" which can be given to Date constructor 
                                     js/Date.)]
      (.toISOString possibly-invalid-date) ; will throw error if date constructed from an invalid string
      possibly-invalid-date)               ; if reach this line, date is valid and returned 
    (catch js/Error _e
      ;; if exception caught, invalid date string, return nil
      nil)))

(comment
  ;; examples from 7GUI
  (string->date "27.03.2014")
  (string->date "27.03.2014x")
  ;; other examples
  (string->date (date->string (js/Date.))))

(def todays-date-string (date->string (js/Date.)))

(def flight-types ["one-way flight" "return flight"])
;; :flight-type may be "one-way flight" or "return flight"
;; using string value for :flight-type so that it can directly be used in HTML elements, either as value or to display
(defonce flight-state (r/atom {:flight-type        "one-way flight"
                               :start-date-string  todays-date-string
                               :return-date-string todays-date-string}))


(defn- return-date-input-disabled? [flight-state]
  (not= "return flight" (:flight-type @flight-state)))

(defn- date-a-before-date-b?
  "Returns true if date-a before date-b"
  [date-a date-b]
  (< (.getTime date-a) (.getTime date-b)))

(defn- valid-booking? 
  "Takes ratom `flight-state` and returns true if booking is valid"
  [flight-state]
  (let [{:keys [flight-type start-date-string return-date-string]} @flight-state
        start-date (string->date start-date-string)
        return-date (string->date return-date-string)]
    (case flight-type
      ;; when "one-way-flight"", booking is valid if start-date is valid (not nil)
      "one-way flight" (when start-date true)
      ;; when "return-flight", booking is valid if
      ;;       both start-date and return-date are valid (not nil), and
      ;;       return-date is not before start-date
      "return flight"  (and start-date 
                           return-date 
                           (not (date-a-before-date-b? return-date start-date))))))

(defn booking-message [flight-state]
  (let [{:keys [flight-type start-date-string return-date-string]} @flight-state
        is-return-flight (= flight-type :return-flight)]
    (str "You have booked a " flight-type " on " start-date-string
         (when is-return-flight (str "returning on " return-date-string)))))

(defn update-date! [flight-state date-string-type date-string]
  (case (:flight-type @flight-state)
    "one-way flight"
    ;;if one-way flight, can only update start-date-string
    (when (= date-string-type :start-date-string)
      (if (string->date date-string)
        ;; if valid date string, also update return-date-string even though it is disabled
        (swap! flight-state merge {:start-date-string  date-string
                                   :return-date-string date-string})
        ;; else string is invalid and don't update return-date-string
        (swap! flight-state merge {:start-date-string  date-string})))
    
    "return flight"
    (swap! flight-state assoc date-string-type date-string)
    ))


(defn date-input
  "Reagent component for a input for date strings in format dd.mm.yyyy.
   `type` may be :start-date-string or :return-date-string.
   If `disabled` is true, field is uneditable and grayed out."
  [date-type disabled]
  (let [date-string (r/cursor flight-state [date-type])
        is-date-string-valid (string->date @date-string)]
    [:input {:class (when-not is-date-string-valid "error")
             :value @date-string
             :disabled disabled
             :on-change #(update-date! flight-state date-type (.. % -target -value))}]))

(defn flight-booker []
  [:div.form
   [:select.input
    {:on-change (fn [e]
                  (swap! flight-state assoc :flight-type (.. e -target -value)))}
    (for [flight-type flight-types]
      ^{:key flight-type} [:option {:value flight-type} flight-type])]
   [:br]
   [date-input :start-date-string false]
   [:br]
   [date-input :return-date-string (return-date-input-disabled? flight-state)]
   [:br]
   [:button
    {:disabled (not (valid-booking? flight-state))
     :on-click #(js/alert (booking-message flight-state))}
    "Book"]])