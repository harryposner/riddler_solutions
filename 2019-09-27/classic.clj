(defn half-inning [strikeout-rate bases-per-hit]
  (loop [runs 0
         outs 0
         base-runners [false false false]]
        (let [strikeout? (< (rand-int (denominator strikeout-rate))
                            (numerator strikeout-rate))]
          (if strikeout?
              (if (= outs 2)
                  runs
                  (recur runs (inc outs) base-runners))
              (let [new-bases (advance-bases bases-per-hit base-runners)
                    new-runs (n-runs base-runners new-bases)]
                (recur new-runs outs new-bases))))))

(defn advance-bases [bases-per-hit base-runners]
  (for [base (range (count base-runners))
        :let [from-base (- base bases-per-hit)] ]
       (cond (= from-base -1) true  ;;; batter
             (< from-base -1) false
             :else (nth base-runners from-base))))

(defn n-runs [old-bases new-bases]
  (let [n-runners (inc (count (filter true? old-bases)))
        on-base (count (filter true? new-bases))]
    (- n-runners on-base)))


(defn game [[home-strikeout-rate home-bases-per-hit]
            [away-strikeout-rate away-bases-per-hit]]
  "True if home team wins else false"
  (loop [home-score 0
         away-score 0
         inning-number 1]
        ;;; Yes, this can play an unnecessary inning.  Whatever.
        (let [new-home-score (+ home-score (half-inning home-strikeout-rate home-bases-per-hit))
              new-away-score (+ away-score (half-inning away-strikeout-rate away-bases-per-hit))]
          (if (and (>= inning-number 9) (not= new-home-score new-away-score))
              (> new-home-score new-away-score)
              (recur new-home-score new-away-score (inc inning-number))))))


(defn season [games-per-matchup]
  (let [moonwalkers [6/10 1]
        doubloons [8/10 2]
        taters [9/10 4]
        mvd (repeatedly games-per-matchup #(game moonwalkers doubloons))
        dvt (repeatedly games-per-matchup #(game doubloons taters))
        tvm (repeatedly games-per-matchup #(game taters moonwalkers))]
    {:moonwalkers (+ (count (filter true? mvd)) (count (filter false? tvm)))
     :doubloons (+ (count (filter true? dvt)) (count (filter false? mvd)))
     :taters (+ (count (filter true? tvm)) (count (filter false? dvt)))}))


(println (season 5000))
