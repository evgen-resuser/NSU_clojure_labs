(ns clojure-labs.integral2)

(defn make-primitive [f h]
  (let [calls (atom 0)            ;; counter of calls for f
        xf (fn [x]
             (swap! calls inc)
             (f x))
        xs (iterate #(+ % h) 0.0)                        ;; creating a lazy sequence 0, h, 2h ...
        fvals (map xf xs)                                   ;; call f+counter on every xs value
        pairs (map vector fvals (rest fvals))               ;; pairs of neighbours - [f(0),f(h)],[f(h),f(2h)]
        acc   (reductions                                   ;; seq of accumulated areas - 0,S1,S2,S3
                (fn [prev [a b]]
                  (+ prev (* h (/ (+ a b) 2.0)))
                )
                0.0
                pairs
              )
        ]
    (fn [x]                                                 ;; return F(x) with closure
      (let [x (double x)]
        (let [steps (int (Math/floor (/ x h)))            ;; int steps
              sn   (nth acc steps)                           ;; trap S: 0 to n
              fnk (nth fvals steps)                         ;; f(n)
              fx  (xf x)                                    ;; f(x) + inc counter
              r   (- x (* steps h))                         ;; difference
              area (* r (/ (+ fnk fx) 2.0))]
          (println "total calls:" @calls ", went to index" steps)
          (+ sn area)
        )
      )
    )
  )
)


(defn integral [f h]
  (let [F (make-primitive f h)]
    (fn [x]
      (- (F x) (F 0))
    )
  )
)

;; пример
(let [f (fn [t] (* t t))
      I (integral f 0.001)
      ]
  (println (I 1))
  (println (I 2))
  (println (I 1.5))
)
