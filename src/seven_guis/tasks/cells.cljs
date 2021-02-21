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

;; state for application

(def num-cols 26)
(def num-rows 100)

(defonce sheet (make-sheet num-cols num-rows))


(defn char->col-num
  [char]
  (- (.charCodeAt char 0) (.charCodeAt \A 0)))

(char->col-num "A")

(defn col-num->char
  [col-num]
  (js/String.fromCharCode (+ (.charCodeAt \A 0) col-num)))


(def identifier->function
  {"add" #(+ %1 %2)
   "sub" #(- %1 %2)
   "div" #(/ %1 %2)
   "mul" #(* %1 %2)
   "mod" #(mod %1 %2)
   "sum" +
   "prod" *})

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
  (let [parsed-formula (parser formula-str)]
    (if (insta/failure? parsed-formula)
      [:invalid]
      parsed-formula)))

(parse-formula "=add(1,2)")
;; => [:formula [:expr [:app [:ident "add"] [:expr [:decimal "1"]] [:expr [:decimal "2"]]]]]

(parse-formula "=sub(B1,B3)")
;; => [:formula [:expr [:app [:ident "sub"] [:expr [:cell "B1"]] [:expr [:cell "B3"]]]]]

(parse-formula "=prod(B1:B3)")
;; [:formula [:expr [:app [:ident "prod"] [:expr [:range [:cell "B1"] [:cell "B3"]]]]]]

(parse-formula "=B1:B3")
;; => [:invalid]

(parse-formula "54")
;; => [:formula [:decimal "54"]]

(apply concat [[1 2]] [[3 4]] [])
;; => ([1 2] [3 4])

(apply concat [[1 2] [3 4]] [])
;; => ([1 2] [3 4])



;; (#(let [c (char->col-num (.charAt % 0))
;;         r (js/parseInt (subs % 1))
;;         key (vector c r)]
;;     key) "Z99")

;; (defn vectorify [& args]
;;   (if (vector? )))

(defn parsed-formula->parsed-formula-with-refs
  "Converts :cell and :range to :refs
   example: [:cell `A1`]                       => [:ref [1 1]]
            [:range [:cell `A1`] [:cell `B2`]] => [:refs [1 1] [2 1] [1 2] [2 2]]"
  [parsed-formula]
  (insta/transform
   {:cell #(let [c (char->col-num (.charAt % 0))
                 r (js/parseInt (subs % 1))
                 key (vector c r)]
             [:ref key])
    :range (fn [cell1 cell2]
             (let [[_:ref [c1 r1]] cell1
                   [_:ref [c2 r2]] cell2]
               (into [:refs] (for [row (range r1 (inc r2))
                                   col (range c1 (inc c2))]
                               [col row]))))}
   parsed-formula))

;; (-> "=A1"
;;     parse-formula
;;     parsed-formula->parsed-formula-with-refs)

;; (-> "=A1:B2"
;;     parse-formula
;;     parsed-formula->parsed-formula-with-refs)


(defn eval-formula
  [sheet parsed-formula]
  (let [parsed-formula-with-refs (parsed-formula->parsed-formula-with-refs parsed-formula)]
    (insta/transform
     #_{:refs    (fn [& keys] (for [key keys]
                              (-> (get sheet key)
                                  deref
                                  (get :value))))}
     {:decimal #(js/parseFloat %)
      :ident   (fn [ident] (identifier->function ident))
      :textual #(constantly 0.0)
      :ref     (fn [key] 
                 (let [cell-atom (get sheet key)] 
                   (:value @cell-atom)))
      :refs    (fn [& keys] (for [key keys]
                              (-> (get sheet key)
                                  deref
                                  (get :value))))
      :app     (fn [f & args]
                 (apply f (flatten args)))
      :expr    identity
      :formula identity}
     parsed-formula-with-refs)))

(->> [3 1]
     (get sheet)
     deref)

(filter (fn [[_ atom]]
          (:value @atom))
        sheet)

(->> "=add(C0,C1)"
     parse-formula
     (eval-formula sheet))

(defn- keys-of-cells-formula-depends-on
  [formula]
  #_(when [parsed-formula (parse-formula formula)]
      (insta/transform
       {:cell #(let [c (char->col-num (.charAt % 0))
                     r (js/parseInt (subs % 1))
                     key (vector c r)]
                 [:refs key])
        :range (fn [cell1 cell2]
                 (let [[_:refs [c1 r1]] cell1
                       [_:refs [c2 r2]] cell2]
                   (into [:refs] (for [row (range r1 (inc r2))
                                       col (range c1 (inc c2))]
                                   [col row]))))
        :decimal (constantly nil)
        :textual (constantly nil)
        :ident   (constantly nil)
        :app     (fn [& args] (into [] (filter identity args)))
        :expr    identity
        :formula identity}
       parsed-formula))
  [])

(parse-formula "=add(B1,B2)")
;; => [:formula [:expr [:app [:ident "add"] [:expr [:cell "B1"]] [:expr [:cell "B2"]]]]]

(keys-of-cells-formula-depends-on "=add(B1,B2)")
;; => ["add"]



;; (parse-formula "=add(B1,prod(B1:C3))")
;; (keys-of-cells-formula-depends-on "=add(B1,prod(B1:C3))")



;; [:formula [:expr [:app [:ident "add"] [:expr [:cell "B1"]] [:expr [:app [:ident "prod"] [:expr [:range [:cell "B1"] [:cell "C3"]]]]]]]]


(for [x nil]
  x)

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
  (println "set-cell-formula called with" formula)
  (let [parsed-formula (parse-formula formula)]
    (doseq [key-of-cell-to-watch (keys-of-cells-formula-depends-on formula)
            :let [cell-to-watch (get sheet key-of-cell-to-watch)]]
      (add-watch cell-to-watch (:key @cell-state)
                 (fn [_key _reference old-state new-state]
                   (when (not= (:value old-state) (:value new-state))
                     (go
                       (->> parsed-formula
                            (eval-formula sheet)
                            (swap! cell-state assoc :value)))))))
    (let [value (eval-formula sheet parsed-formula)]
      (println "formula =" formula "value =" value)
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
                      (println "on-blur called. e.target.value=" (.. e -target -value))
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
     ^{:key key} [cell sheet key])])

(defn cells []
  (let []
    (fn []
      [:div
       [:table
        [:thead
         [:tr
          [:th nil]
          (let [char-vec (map col-num->char (range num-cols))]
            (for [char char-vec]
              ^{:key char} [:th char]))]]
        [:tbody
         (for [r (range num-rows)]
           ^{:key r} [row sheet r])]]])))