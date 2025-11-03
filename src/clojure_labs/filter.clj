(ns clojure-labs.filter)

(defn split [coll n]
  (let [total   (count coll)
        base    (int (/ total n))
        remain  (mod total n)
        ]
    (loop [rest-coll coll
           i 0                                              ;; current part
           result []                                        ;; result with all parts
           ]
      (if (< i n)
        (let [extra (if (< i remain) 1 0)
              size (+ base extra)
              current (take size rest-coll)                 ;; first N elems
              tail    (drop size rest-coll)                 ;; seq made from coll without first N elems
              ]
          (recur tail (inc i) (conj result current))        ;; do ut again with recur
        )
        result
      )
    )
  )
)

(defn future-filter [pred coll blocks-count]
  (let [parts   (split coll blocks-count)
        futures (map
                  (fn [part]
                     (future (doall (filter pred part)))
                  )
                  parts
                )
        results (map deref futures)]                      ;; wait for all futures end
    (apply concat results)))


;; тесты

(defn measure-time [f]
  (let [start (System/nanoTime)
        result (f)
        end   (System/nanoTime)]
    [result (/ (- end start) 1e6)]))

(defn -main []
  ;; Создаём большую коллекцию случайных чисел
  (let [data (repeatedly 10000 #(rand-int 1000000))
        pred (fn [x]
               (Thread/sleep 1)
               (= 0 (mod x 7))
             )
        ]

    (println "Coll size:" (count data))

    (let [[res1 time1] (measure-time #(doall (filter pred data)))]
      (println "\nfilter:")
      (println "  after filtering:" (count res1))
      (println "  time ms:" (format "%.10f" time1)))

    (let [[res2 time2] (measure-time #(doall (future-filter pred data 10)))]
      (println "\nfuture-filter (10):")
      (println "  after filtering:" (count res2))
      (println "  time ms:" (format "%.10f" time2)))

    (let [[res3 time3] (measure-time #(doall (future-filter pred data 20)))]
      (println "\nfuture-filter (20):")
      (println "  after filtering:" (count res3))
      (println "  time ms:" (format "%.10f" time3)))
    ))

