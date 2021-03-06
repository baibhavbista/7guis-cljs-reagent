;; This test runner is intended to be run from the command line
(ns seven-guis.test-runner
  (:require
    ;; require all the namespaces that you want to test
   [seven-guis.tasks.cells-test]
   [seven-guis.tasks.tempr-converter-test]
   [figwheel.main.testing :refer [run-tests-async]]))

(defn -main [& args]
  (run-tests-async 5000))
