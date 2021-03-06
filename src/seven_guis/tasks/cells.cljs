(ns seven-guis.tasks.cells
  (:require [reagent.core :as r]
            [instaparse.core :as insta]
            [clojure.string :as string]
            [cljs.core.async :refer [go]]))

;;; The inspiration for this task is the SCells spreadsheet example from the book Programming in Scala.
;;; Link: https://www.artima.com/pins1ed/the-scells-spreadsheet.html
;;; Since the above uses a parser, I will be using a parsing library for this too
;;; Library of choice is Instaparse (https://github.com/Engelberg/instaparse), one of the most popular parsing libraries for Clojure(Script)

;;; Full disclosure: I was stuck on a UI problem while doing this task. After making no headway even after a few days, I searched 
;;; for other's solutions to see how they had solved that problem and so, some parts of UI, especially the representation of 
;;; cells as standalone atoms and the building up of the sheet-state were heavily inspired from:
;;; https://github.com/polymeris/7guis-clojurescript/blob/master/src/viiguis/guis/cells.cljs
;;; I tried to code it again from scratch but the code came out nearly the same as his. Maybe future me will revisit this problem 
;;; and solve it in a different manner.


;; a separate atom for each cell so that we can use atom watchers for change propagation 
(defn- make-cell-state
  "Returns a ratom corresponding to cell wth key `key`"
  [key]
  (r/atom {:key key
           :formula nil
           :value nil}))

(defn make-sheet
  "Returns a sheet of num-rows by num-cols. Each cell is an atom with its state"
  [num-cols num-rows]
  (->> (for [c (range num-cols)
             r (range num-rows)
             :let [key       (vector c r)
                   cell-atom (make-cell-state key)]]
         (vector key cell-atom))
       (into {})))

(def num-cols 26)  ; A-Z
(def num-rows 100) ; 0-99

;; state for application
(defonce sheet (make-sheet num-cols num-rows))


(defn char->col-num
  [char]
  (- (.charCodeAt char 0) (.charCodeAt \A 0)))

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
  ;; input to insta/parser is the context-free grammar for our spreadsheet language
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

(defn- parsed-formula->parsed-formula-with-refs
  "Converts :cell to :ref and and :range to :refs 
   example: [:cell `A1`]                       => [:refs [1 1]]
            [:range [:cell `A1`] [:cell `B2`]] => [:refs [1 1] [2 1] [1 2] [2 2]]
   Usually called with the output of `parser` i.e. `parsed-formula`"
  [parsed-formula]
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
                               [col row]))))}
   parsed-formula))

(defn parsed-formula-with-refs [formula-str]
  "Given a formula string `formula-str`, parses and then converts cells and ranges to refs.
   If failure to parse, returns an ExceptionInfo object with ex-message 'ERR!' and detailed info in :detailed-error"
  (let [parsed-formula (parser formula-str)]
    (if (insta/failure? parsed-formula)
      (ex-info "ERR!" {:detailed-error (str (insta/get-failure parsed-formula))})
      (parsed-formula->parsed-formula-with-refs parsed-formula))))

(defn eval-formula
  "Evaluated `parsed-formula-with-refs` using value of cells in sheet"
  [sheet parsed-formula-with-refs]
  (if (ex-message parsed-formula-with-refs)
    parsed-formula-with-refs                ; if error while parsing return ExceptionInfo object directly
    (insta/transform
     ;; insta/transform expects a map of functions for each type in the CFG, which it applies to transform the parsed tree.
     ;; Here, functions are supplied as to evaluate the value of the formula
     {:decimal js/parseFloat
      :ident   (fn [ident] (identifier->function (string/lower-case ident)))
      :textual identity
      :refs    (fn [& keys]
                 (for [key keys]
                   (-> (get sheet key)
                       deref
                       (get :value))))
      :app     (fn [f & args]
                 (apply f (flatten args)))
      :expr    identity
      :formula identity}
     parsed-formula-with-refs)))

(defn keys-of-cells-formula-depends-on
  "Returns a list of the keys of the cells given `formula` depends on.
   If parser error, returns nil.
   E.g. given formula `=add(B1,B2)` returns ([1 1] [1 2])"
  [formula]
  (when formula
    (let [parsed-formula-with-refs (parsed-formula-with-refs formula)]
      (when-not (ex-message parsed-formula-with-refs)  ; return nil when parser error (when parsed-formula-with-refs returns ExceptionInfo) 
        (insta/transform
         {:decimal (constantly nil)
          :textual (constantly nil)
          :ident   (constantly nil)
          :refs    (fn [& args] (vec args))
          :app     (fn [& args] (apply concat args))
          :expr    identity
          :formula identity}
         parsed-formula-with-refs)))))

(defn remove-cell-watches!
  "Removes watches on the cell. should be called before changing cell formula"
  [sheet cell-state]
  (let [{:keys [formula key]} @cell-state]
    #_(println "formula =" formula "key =" key)
    (doseq [watched-cell-key (keys-of-cells-formula-depends-on formula)
            :let [watched-cell (get sheet watched-cell-key)]]
      (remove-watch watched-cell key))))

(defn set-cell-formula!
  "Sets a new formula for the cell. Removes old watches and adds watchers to the cells the new formula depends on."
  [sheet key formula]
  #_(println "set-cell-formula! called with" key formula)
  (let [cell-state (get sheet key)]
    (remove-cell-watches! sheet cell-state)
    (let [parsed-formula-with-refs (parsed-formula-with-refs formula)]
      (doseq [key-of-cell-to-watch (keys-of-cells-formula-depends-on formula)
              :let [cell-to-watch (get sheet key-of-cell-to-watch)]]
        (add-watch cell-to-watch (:key @cell-state)
                   (fn [_key _reference old-state new-state]
                     (when (not= (:value old-state) (:value new-state))
                       (go
                         (->> parsed-formula-with-refs
                              (eval-formula sheet)
                              (swap! cell-state assoc :value)))))))
      (let [value (eval-formula sheet parsed-formula-with-refs)]
        #_(println "formula =" formula "value =" value)
        (println value)
        (swap! cell-state 
               assoc 
               :formula formula
               :value   value)))))

;;; REAGENT COMPONENTS
(defn cell
  "Reagent component for a single cell in the sheet"
  [sheet key]
  (let [cell-state (get sheet key)
        {:keys [value formula]} @cell-state
        error-message (ex-message value)]
    [:td
     [:input
      {:placeholder (str (or error-message value))
       :on-focus (fn [e]
                   (set! (.. e -target -value)
                         (or formula value)))
       :on-blur (fn [e]
                  #_(println "on-blur called. e.target.value=" (.. e -target -value))
                  (when-not (string/blank? (.. e -target -value))
                    (set-cell-formula! sheet key (.. e -target -value))
                    (set! (.. e -target -value) nil)))
       :on-key-press (fn [e]
                       (when (= "Enter" (.-key e))
                         (.preventDefault e)
                         (.blur (.-target e))))
       :class (when error-message "error")}]]))

(defn row
  "Reagent component for a row in the sheet"
  [sheet row-n]
  [:tr
   [:td row-n]
   (for [c (range num-cols)
         :let [key (vector c row-n)]]
     ^{:key key} [cell sheet key])])

(defn cells
  "Reagent component for task 7: Cells"
  []
  [:div.cells--wrapper
   [:table.cells--table
    [:thead
     [:tr
      [:th nil]
      (let [char-vec (map col-num->char (range num-cols))]
        (for [char char-vec]
          ^{:key char} [:th char]))]]
    [:tbody
     (for [r (range num-rows)]
       ^{:key r} [row sheet r])]]])