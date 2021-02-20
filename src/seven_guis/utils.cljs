(ns seven-guis.utils)

(defn input-event->value
  "Extracts value from input event `e`.
   Equivalent to (-> e .-target .-value)"
  [e]
  (-> e .-target .-value))

(defn vecmap-assoc-pred
  "Matches the first map in the vector of maps `vm` that satisfies given predicate `pred` and maps `key` to `val` in the matched map"
  [vm pred key val]
  (let [i (some (fn [[i m]] (when (pred m) i))
                (map-indexed vector vm))]
    (update vm i #(assoc % key val))))