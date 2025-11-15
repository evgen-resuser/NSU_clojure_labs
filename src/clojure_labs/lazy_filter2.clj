(ns clojure-labs.lazy-filter2)


(defn split [n coll]
  (if (not (empty? coll))
      (lazy-seq (cons (take n coll) (split n (drop n coll))))
      ()
    )
  )

(defn filter-part [pred coll part-size threads]
  (->> (split part-size coll)                               ;; ((1 2) (3 4) (5 6)...) if part-size=2
       (split threads)                                      ;; (1 2) (3 4) (5 6) | (7 8) (9 10) (11 12) | ... if threads=3
       (map (fn [part]
              (->> (map #(future (doall (filter pred %))) part)
                   (doall threads)
                   (map deref)
               )
          )
        )
       (apply concat)
       (apply concat)                                       ;; ((((filter result) (filter result)) ...)
       )
  )

(defn lazy-filter
  ([pred coll part-size] (filter-part pred coll part-size (.availableProcessors (Runtime/getRuntime))))
  ([pred coll part-size threads] (filter-part pred coll part-size threads))
  )

(defn measure-time [f]
  (let [start (System/nanoTime)
        result (f)
        end   (System/nanoTime)]
    [result (/ (- end start) 1e6)]))

(defn -main []
  (let [calls (atom 0)
        pred  (fn [x]
                (swap! calls inc)
                (= 0 (mod x 7))
              )
        lazy-coll (iterate inc 0)
        lazy-filtered (lazy-filter pred lazy-coll 5 2)
        ]

    (let [res (take 10 lazy-filtered)]
      (println "\nResult:" res)
      (println "  Predicate calls:" @calls)
    )
    (let [res (take 5 lazy-filtered)]
      (println "\nResult:" res)
      (println "  Predicate calls:" @calls)
    )
    (let [res (take 12 lazy-filtered)]
      (println "\nResult:" res)
      (println "  Predicate calls:" @calls)
    )
  )
  (let [data (repeatedly 5000 #(rand-int 1000000))
        pred (fn [x]
               (Thread/sleep 1)
               (= 0 (mod x 7))
               )
        ]

    (println "\n=============\nColl size:" (count data))

    (let [[res1 time1] (measure-time #(doall (filter pred data)))]
      (println "\nfilter:")
      (println "  after filtering:" (count res1))
      (println "  time ms:" (format "%.10f" time1))
      )

    (let [[res2 time2] (measure-time #(doall (lazy-filter pred data 10 5)))]
      (println "\nlazy-filter:")
      (println "  after filtering:" (count res2))
      (println "  time ms:" (format "%.10f" time2))
      )
    )
)
