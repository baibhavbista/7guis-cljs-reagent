(ns seven_guis.tasks.cells-test
  (:require
   [cljs.test :refer-macros [deftest is are testing run-tests]]
   [seven-guis.tasks.cells :refer [col-num->char 
                                   char->col-num 
                                   parsed-formula-with-refs
                                   keys-of-cells-formula-depends-on]]))

(deftest char-column-number-conversion-tests
  (are [char col] (= char (col-num->char col))
    "A" 0
    "Z" 25)
  (are [col char] (= col (char->col-num char))
    0 "A"
    25 "Z"))

(deftest parsed-formula-with-refs-tests
  (are [formula-str parsed] (= parsed (parsed-formula-with-refs formula-str))

    "3/C1"
    [:formula [:textual "3/C1"]]

    "=add(1,2)"
    [:formula [:expr [:app [:ident "add"] [:expr [:decimal "1"]] [:expr [:decimal "2"]]]]]

    "=sub(B1,B3)"
    [:formula [:expr [:app [:ident "sub"] [:expr [:refs [1 1]]] [:expr [:refs [1 3]]]]]]
    
    "=prod(B1:B3)"
    [:formula [:expr [:app [:ident "prod"] [:expr [:refs [1 1] [1 2] [1 3]]]]]]

    "=B1:B3"
    [:formula [:expr [:refs [1 1] [1 2] [1 3]]]]
    
    "54"
    [:formula [:decimal "54"]]
    
    "=add(sum(A1:B2),B3)"
    [:formula [:expr [:app [:ident "add"] [:expr [:app [:ident "sum"] [:expr [:refs [0 1] [1 1] [0 2] [1 2]]]]] [:expr [:refs [1 3]]]]]]))

(deftest keys-of-cells-formula-depends-on-tests
  (are [formula-str keys] (= keys (keys-of-cells-formula-depends-on formula-str))
    
    "=add(B1,B2)"
    '([1 1] [1 2])
    
    "=sum(add(A1,sum(A3:A9)),B3,C11)"
    '([0 1] [0 3] [0 4] [0 5] [0 6] [0 7] [0 8] [0 9] [1 3] [2 11])
    
    "=add(B1,prod(B1:C3))"
    '([1 1] [1 1] [2 1] [1 2] [2 2] [1 3] [2 3])))