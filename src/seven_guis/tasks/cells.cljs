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

(defn parsed-formula->parsed-formula-with-refs
  "Converts :cell to :ref and and :range to :refs 
   example: [:cell `A1`]                       => [:refs [1 1]]
            [:range [:cell `A1`] [:cell `B2`]] => [:refs [1 1] [2 1] [1 2] [2 2]]"
  [parsed-formula]
  (insta/transform
   {:cell #(let [c (char->col-num (.charAt % 0))
                 r (js/parseInt (subs % 1))
                 key (vector c r)]
             [:refs key])
    :range (fn [cell1 cell2]
             (let [[_:ref [c1 r1]] cell1
                   [_:ref [c2 r2]] cell2]
               (into [:refs] (for [row (range r1 (inc r2))
                                   col (range c1 (inc c2))]
                               [col row]))))}
   parsed-formula))

(defn parsed-formula-with-refs [formula-str]
  (let [parsed-formula (parser formula-str)]
    (if (insta/failure? parsed-formula)
      (ex-info "ERR!" {:detailed-error (str (insta/get-failure parsed-formula))})
      (parsed-formula->parsed-formula-with-refs parsed-formula))))



(parsed-formula-with-refs "3/C1")

;; (parse-formula "=add(1,2)")
;; ;; => [:formula [:expr [:app [:ident "add"] [:expr [:decimal "1"]] [:expr [:decimal "2"]]]]]

;; (parse-formula "=sub(B1,B3)")
;; ;; => [:formula [:expr [:app [:ident "sub"] [:expr [:cell "B1"]] [:expr [:cell "B3"]]]]]

;; (parse-formula "=prod(B1:B3)")
;; ;; [:formula [:expr [:app [:ident "prod"] [:expr [:range [:cell "B1"] [:cell "B3"]]]]]]

;; (parse-formula "=B1:B3")
;; ;; => [:invalid]

;; (parse-formula "54")
;; ;; => [:formula [:decimal "54"]]

;; (apply concat [[1 2]] [[3 4]] [])
;; ;; => ([1 2] [3 4])

;; (apply concat [[1 2] [3 4]] [])
;; ;; => ([1 2] [3 4])



;; (#(let [c (char->col-num (.charAt % 0))
;;         r (js/parseInt (subs % 1))
;;         key (vector c r)]
;;     key) "Z99")

;; (defn vectorify [& args]
;;   (if (vector? )))

;; (-> "=A1"
;;     parse-formula
;;     parsed-formula->parsed-formula-with-refs)

;; (-> "=add(sum(A1:B2),B3)"
;;     parse-formula
;;     parsed-formula->parsed-formula-with-refs)


(defn eval-formula
  [sheet parsed-formula-with-refs]
  (if (ex-message parsed-formula-with-refs)
    parsed-formula-with-refs                         ; if error while parsing return ExceptionInfo object directly
    (insta/transform
     #_{:refs    (fn [& keys] (for [key keys]
                                (-> (get sheet key)
                                    deref
                                    (get :value))))}
     {:decimal js/parseFloat
      :ident   (fn [ident] (identifier->function (string/lower-case ident)))
      :textual identity
      :ref     (fn [key]
                 (let [cell-atom (get sheet key)]
                   (:value @cell-atom)))
      :refs    (fn [& keys]
                 (for [key keys]
                   #_(let [cell-atom (get sheet key)]
                       (:value @cell-atom))
                   (-> (get sheet key)
                       deref
                       (get :value))))
      :app     (fn [f & args]
                 (apply f (flatten args)))
      :expr    identity
      :formula identity}
     parsed-formula-with-refs)))

;; (eval-formula sheet [:formula [:textual "3/C1"]])

;; (->
;;  (parsed-formula-with-refs "3/C1")
;;  ;; => [:formula [:textual "3/C1"]]

;;  (eval-formula sheet))

;; (flatten [[2] [3]])
;; (flatten [[2 3]])


;; (filter (fn [[_ atom]]
;;           (:value @atom))
;;         sheet)

;; (->> "4"
;;      parse-formula
;;      (eval-formula sheet))

;; (->> "=add(C0,C1)"
;;      parse-formula
;;      (eval-formula sheet))

;; (->> "=sum(C0:C2)"
;;      parse-formula
;;      (eval-formula sheet))


(defn- keys-of-cells-formula-depends-on
  [formula]
  #_(->> formula
       parse-formula
       parsed-formula->parsed-formula-with-refs
       (insta/transform))
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
         parsed-formula-with-refs))))
  #_[])

;; (parsed-formula-with-refs "3")
;; ;; => [:formula [:decimal "3"]]

;; (keys-of-cells-formula-depends-on nil)

;; (concat nil [[1 2] [1 3] [1 5]])
;; ;; => ([1 2] [1 3] [1 5])

;; (concat nil [[1 2]] [[1 3]])
;; ;; => ([1 2] [1 3])


;; (parse-formula "=add(B1,B2)")
;; ;; => [:formula [:expr [:app [:ident "add"] [:expr [:cell "B1"]] [:expr [:cell "B2"]]]]]

;; (keys-of-cells-formula-depends-on "=add(B1,B2)")
;; ;; => [[[1 1]] [[1 2]]]

;; (keys-of-cells-formula-depends-on "=sum(add(A1,sum(A3:A9)),B3,C11)")
;; [[[[0 1] [1 1] [0 2] [1 2]]] [[1 3]]]

;; (parsed-formula-with-refs nil)



;; (parse-formula "=add(B1,prod(B1:C3))")
;; (keys-of-cells-formula-depends-on "=add(B1,prod(B1:C3))")



;; [:formula [:expr [:app [:ident "add"] [:expr [:cell "B1"]] [:expr [:app [:ident "prod"] [:expr [:range [:cell "B1"] [:cell "C3"]]]]]]]]



(defn remove-cell-watches!
  "Removes watches on the cell. should be called before changing cell formula"
  [sheet cell-state]
  (let [{:keys [formula key]} @cell-state]
    #_(println "formula =" formula "key =" key)
    (doseq [watched-cell-key (keys-of-cells-formula-depends-on formula)
            :let [watched-cell (get sheet watched-cell-key)]]
      (remove-watch watched-cell key))))

(defn set-cell-formula!
  [sheet cell-state formula]
  #_(println "set-cell-formula called with" cell-state formula)
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
                      #_(println "on-blur called. e.target.value=" (.. e -target -value))
                      (when-not (string/blank? (.. e -target -value))
                        (set-cell-formula! sheet cell-state (.. e -target -value))
                        (set! (.. e -target -value) nil)))
           :on-key-press (fn [e]
                           (when (= "Enter" (.-key e))
                             (.preventDefault e)
                             (.blur (.-target e))))
           :class (when error-message "error")}]]))))

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
      [:div.cells--wrapper
       [:table.cells-table
        [:thead
         [:tr
          [:th nil]
          (let [char-vec (map col-num->char (range num-cols))]
            (for [char char-vec]
              ^{:key char} [:th char]))]]
        [:tbody
         (for [r (range num-rows)]
           ^{:key r} [row sheet r])]]])))