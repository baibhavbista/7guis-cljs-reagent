(ns seven-guis.tasks.cells
  (:require [reagent.core :as r]
            [instaparse.core :as insta]
            [clojure.string :as string]
            [cljs.core.async :refer [go]]))

;;; The inspiration for this task is the SCells spreadsheet example from the book Programming in Scala.
;;; Link: https://www.artima.com/pins1ed/the-scells-spreadsheet.html
;;; So, I will be using a parsing library for this too

;;; Parsing stuff
;;; parser returns a "tree"
;;; eval/return the cells current formula depends on can be done by either sth like clojure.walk/post-walk or insta/transform 
;;; look at this later
(def parser
  (insta/parser "
    formula = decimal / textual / (<'='> expr)
    expr    = range / cell / decimal / app
    app     = ident <'('> (expr <','>)* expr <')'>
    range   = cell <':'> cell
    cell    = #'[A-Za-z]\\d+'
    textual = #'[^=].*'
    ident   = #'[a-zA-Z_]\\w*'
    decimal = #'-?\\d+(\\.\\d*)?'
    "))

(defn parse-formula [formula-str]
  (let [parsed-formula-struct (parser formula-str)]
       (if (insta/failure? parsed-formula-struct)
         [:invalid]
         parsed-formula-struct)))

(parse-formula "=add(1,2)")
;; => [:formula [:expr [:app [:ident "add"] [:expr [:decimal "1"]] [:expr [:decimal "2"]]]]]

(parse-formula "=sub(B1,B3)")
;; => [:formula [:expr [:app [:ident "sub"] [:expr [:cell "B1"]] [:expr [:cell "B3"]]]]]

(parse-formula "=prod(B1:B3)")
;; => [:formula [:expr [:app [:ident "prod"] [:expr [:range [:cell "B1"] [:cell "B3"]]]]]]

(parse-formula "=B1:B3")
;; => [:invalid]

;; (parse-formula "54")

;; state for application

(def num-cols 26)
(def num-rows 100)

(defn eval-formula
  [sheet parsed-formula-struct]
  0)

(defn- keys-of-cells-formula-depends-on
  [formula]
  [])

(defn remove-cell-watches!
  "Removes watches on the cell. Call before changing cell formula or value"
  [sheet cell-state]
  (let [{:keys [formula key]} @cell-state]
    (doseq [watched-cell-key (keys-of-cells-formula-depends-on formula)
            :let [watched-cell (get sheet watched-cell-key)]]
      (remove-watch watched-cell key))))

(defn set-cell-formula!
  [sheet cell-state formula]
  (remove-cell-watches! sheet cell-state)
  (let [parsed-formula-struct (parse-formula formula)]
    (doseq [key-of-cell-to-watch (keys-of-cells-formula-depends-on formula)
            :let [cell-to-watch (get sheet key-of-cell-to-watch)]]
      (add-watch cell-to-watch (:key @cell-state)
                 (fn [_key _reference old-state new-state]
                   (when (not= (:value old-state) (:value new-state))
                     (go
                       (->> parsed-formula-struct
                            (eval-formula sheet)
                            (swap! cell-state assoc :value)))))))
    (let [value (eval-formula sheet parsed-formula-struct)]
      (swap! cell-state assoc :formula formula
                              :value   value))))

(defn cell
  [sheet key]
  (let [cell-state (get sheet key)]
    (fn [sheet key]
      (let [{:keys [value formula]} @cell-state
            error-message (ex-message value)]
        [:td
         [:input
          {:placeholder (str (or error-message value))
           :on-focus (fn [e] 
                       (set! (.. e -target -value)
                             (or formula value)))
           :on-blur (fn [e]
                      (when-not (string/blank? (.. e -target -value))
                        (set-cell-formula! sheet cell-state (.. e -target -value))
                        (set! (.. e -target -value) nil)))
           :on-key-press (fn [e]
                           (when (= "Enter" (.-key e))
                             (.preventDefault e)
                             (.blur (.-target e))))}]]))))

(defn row
  [sheet row-n]
  [:tr
   [:td row-n]
   (for [c (range num-cols)
         :let [key (vector c row-n)]]
     ^{:key key} [cell sheet key] #_[:td [:input]])])

;; a separate atom for each cell as then we can use atom watchers for change propagation 
(defn- make-cell-state
  [key]
  (r/atom {:key key
           :formula nil
           :value nil}))

(defn make-sheet
  [num-cols num-rows]
  (->> (for [c (range num-cols)
             r (range num-rows)
             :let [cell-atom (make-cell-state [c r])
                   key       (:key @cell-atom)]]
         (vector key cell-atom))
       (into {})))

;; (make-sheet 26 100)

(defn cells []
  (let [sheet (make-sheet num-cols num-rows)]
    (fn []
      [:div
       [:table
        [:thead 
         [:tr
          [:th nil]
          (let [char-vec (map (fn [i] (js/String.fromCharCode (+ 65 i)))
                              (range num-cols))]
            (for [char char-vec]
              ^{:key char} [:th char]))]]
        [:tbody
         (for [r (range num-rows)]
           ^{:key r} [row sheet r])]]])))