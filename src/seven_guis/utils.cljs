(ns seven-guis.utils)

(defn input-event->value
  "Extracts value from input event `e`.
   Equivalent to (-> e .-target .-value)"
  [e]
  (-> e .-target .-value))