(ns clojure-labs.core)

;; 1.1 simple recursion
(defn generate-strings [symbols n]
  (cond
    (= n 0) '("")

    (= n 1) (map str symbols)                               ;; array of chars

    :else
    (let [shorter-strings (generate-strings symbols (dec n))]
      (for [string shorter-strings                          ;; for (let string of shorter-strings)
            symbol symbols                                  ;; for (let symbol of symbols)
            :when (not= (str symbol) (str (last string)))]
        (str string symbol)
        )
      )
    )
  )

;; 1.2 tail recursion
(defn generate-strings-tail [symbols n]
  (letfn [
          (build-strings [current-length acc]
            (if (= current-length n)
              acc
              (let [next-strings
                    (if (= current-length 0)
                      (map str symbols)                     ;; array of chars
                      (for [string acc
                            symbol symbols
                            :when (not= (str symbol) (str (last string)))]
                        (str string symbol)))]
                (recur (inc current-length) next-strings)
                )
              )
            )
          ]
    ;; body
    (if (= n 0)
      '("")
      (build-strings 0 [])
      )
    )
  )

;; 1.3.1
(defn my-map [callback coll]
  (reduce
    (fn [acc elem]
      (conj acc (callback elem))
      )
    '()
    coll
    )
  )

;; 1.3.2
(defn my-filter [callback coll]
  (reduce
    (fn [acc elem]
      (if (callback elem)
        (conj acc elem)
        acc
        )
      )
    '()
    coll
    )
  )

;; 1.4

(defn extend-strings [symbols strings]
  (reduce                                                   ;; walk through the strings
    (fn [acc string]
      (concat acc                                           ;; concat prev and new strings
        (filter identity  ;: filter non nil with identity callback: (a) => a (everything is truly except nil and false, they are falsy)
          (map
           (fn [symbol]
             (when (or (empty? string) (not= (str symbol) (str (last string))))
               (str string symbol)
             )                                              ;; nil if not
           )
           symbols
         )
        )
      )
    )
  []
  strings
  )
)

(defn generate-strings-elem-ops [symbols n]
  (cond
    (= n 0) '("")
    (= n 1) (map str symbols)
    :else
    (reduce                                                 ;; extend strs n-1 times
      (fn [strings _]
        (extend-strings symbols strings)
      )
      (map str symbols)                                     ; val (just symbs list)
      (range 1 n)                                           ; col ( (1...n) ) - do n-1 times
    )
  )
)

;; tests
(let [abc ["a", "b", "c"]]

  (let [result (generate-strings abc 3)]
    (println result)
    )

  (let [result1 (generate-strings-tail abc 3)]
    (println result1)
    )

  (let [result2 (generate-strings-elem-ops abc 3)]
    (println result2)
    )
  )

(let [ints [1, 2, 3, 4, 5, 6, 7]]

  (println (my-map inc ints))

  (println (my-filter #(> % 5) ints))

  )