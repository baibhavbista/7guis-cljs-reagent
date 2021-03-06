(ns seven-guis.tasks.tempr-converter-test
  (:require
   [cljs.test :refer-macros [deftest is are testing run-tests]]
   [seven-guis.tasks.tempr-converter :refer [f->c c->f]]))

(deftest tempr-conversion-tests
  (are [deg-c deg-f] 
       (and (= deg-f (c->f deg-c))
            (= deg-c (f->c deg-f)))
    0   32
    100 212
    ))