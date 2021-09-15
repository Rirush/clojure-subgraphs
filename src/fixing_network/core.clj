(ns fixing-network.core)

(defn any-of? [coll values]
  (some #(contains? coll %) values))

(defn -main []
  (let [file (slurp "input.txt")
        lines (clojure.string/split-lines file)
        n (Integer/parseInt (first lines))
        connections (for [line (rest lines)]
                      (vec
                        (map #(Integer/parseInt %)
                             (clojure.string/split line #" "))))
        connections-map (apply merge-with
                               (partial apply conj)
                               (sorted-map)
                               (into {} (for [i (range n)] [(+ i 1) []]))
                               (map
                                 (fn [v] {(first v) [(second v)] (second v) [(first v)]})
                                 connections))
        flattened-connections (map #(set (flatten %)) connections-map)
        result (loop [result []
                      connections flattened-connections]
                 (if (empty? connections)
                   result
                   (let [[good separated] (reduce
                                            (fn [[good filtered] values]
                                              (if (any-of? good values)
                                                [(apply conj good values) filtered]
                                                [good (conj filtered values)]))
                                            [(set (first connections)) []] connections)]
                     (recur (conj result good) separated))))]
    (println (- (count result) 1))))
