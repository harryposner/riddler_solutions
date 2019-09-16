(require '[clojure.set :refer [union]])


(def all-states #{"AL" "AK" "AS" "AZ" "AR" "CA" "CO" "CT" "DE" "DC" "FM" "FL"
                  "GA" "GU" "HI" "ID" "IL" "IN" "IA" "KS" "KY" "LA" "ME" "MH"
                  "MD" "MA" "MI" "MN" "MS" "MO" "MT" "NE" "NV" "NH" "NJ" "NM"
                  "NY" "NC" "ND" "MP" "OH" "OK" "OR" "PW" "PA" "PR" "RI" "SC"
                  "SD" "TN" "TX" "UT" "VT" "VI" "VA" "WA" "WV" "WI" "WY"})

(def with-mil (union all-states #{"AE" "AP" "AA"}))


(defn abbreviation-tree [state chain remaining-states]
  (let [next-states (filter #(= (last state) (first %)) remaining-states)
        next-chain (conj chain state) ]
    (if (empty? next-states)
        [:leaf next-chain]
        [:branch (map #(abbreviation-tree % next-chain (disj remaining-states %))
                      next-states)])))

(defn leaf? [node] (= :leaf (first node)))
(defn branch? [node] (= :branch (first node)))
(defn children [node] (second node))
(defn leaf-contents [node] (second node))

(defn all-chains [tree]
  (->> tree
       (tree-seq branch? children)
       (filter leaf?)
       (map leaf-contents)))

(defn longest-chain [chains]
  (loop [remaining chains
         best nil
         best-count 0]
    (let [current (first remaining)
          current-count (count current)]
      (cond
        (empty? remaining) best
        (> current-count best-count) (recur (rest remaining) current current-count)
        :else (recur (rest remaining) best best-count)))))

(defn all-chains-for-state [states state]
  (-> state
      (abbreviation-tree [] (disj states state))
      all-chains))

(defn longest-chain-for-state [states state]
  (longest-chain (all-chains-for-state states state)))

(defn longest-chain-for-state! [all-longest-atom states state]
  (let [chain (longest-chain-for-state states state)]
    (swap! all-longest-atom conj chain)
    chain))

(defn longest-chains-all-states! [all-longest-atom states]
  (->> states
       (pmap (partial longest-chain-for-state! all-longest-atom states))
       longest-chain))

(defn chain->string [chain]
  (reduce str (first chain) (map last (rest chain))))

(defn print-longest [states]
  (let [all-longest (atom [])
        longest (longest-chains-all-states! all-longest states)]
    (println "The longest chain we can make is:")
    (println (chain->string longest))
    (println)
    (println "The longest for each state is:")
    (doall (map #(println (first %) ":" (chain->string %))
                (sort-by first @all-longest)))))

;;; Main
(println "Including only state/possessions:")
(print-longest all-states)
(println)
(println "Including the military postal abbreviations:")
(print-longest with-mil)
