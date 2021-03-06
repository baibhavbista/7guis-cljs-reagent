(ns seven_guis.tasks.cells-test
  (:require
   [cljs.test :refer-macros [deftest is are testing run-tests]]
   [seven-guis.tasks.cells :refer [col-num->char char->col-num]]))

(deftest char-column-number-conversion-tests
  (are [char col] (= char (col-num->char col))
    "A" 0
    "Z" 25)
  (are [col char] (= col (char->col-num char))
    0 "A"
    25 "Z"))