(ns clojure-labs.integral)

(defn trapecii [f h]                         ;; f - f(t), h - step
  (let [cache (atom {0 0.0})]                              ;; atom with map x -> val | upper-boundary => I(from 0 to upper-boundary)
    (fn [t]                                                 ;; returns F(t)
      (let [m @cache                                        ;; get map from cahce
          [nearest-t nearest-F] (apply min-key (fn [entry] (abs (- (key entry) t))) m) ;; (min-key fn col) returns an arg x where fn(x) is min
          dir (if (>= t nearest-t) 1 -1)
          step (* dir h)
          dist (abs (- t nearest-t))
          steps (int (/ dist h))
          remain (- dist (* steps h))                     ;; what can not be covered with h
          acc
          (loop [i 0
                 sum 0.0
                 prev-y (f nearest-t)
                 x nearest-t
                 ]
            (if (< i steps)
              (let [x-next (+ x step)                     ;; have some int steps
                    y (f x-next)
                    inc-val (* 0.5 h (+ prev-y y))]       ;; trap S
                (swap! cache (fn [old-m] (assoc old-m x-next (+ nearest-F sum inc-val)))) ;; save next arg and value
                (recur (inc i) (+ sum inc-val) y x-next))
              (if (pos? remain)                           ;; no int steps left -> need to add some or not
                (let [x-before (+ nearest-t (* dir steps h))
                      inc-last (* 0.5 remain (+ (f x-before) (f t)))
                      ]
                  (+ sum inc-last)
                )
                sum
              )
            )
          )
        ]
        (let [res (+ nearest-F acc)]                        ;; F(t) = F(nearest) + acc
          (swap! cache (fn [old-m] (assoc old-m t res)))
          res
        )
      )
    )
  )
)


(defn integral [f h]
  (let [F (trapecii f h)]
    (fn [x]
      (- (F x) (F 0))
    )
  )
)

(let [f (fn [t] (* t t)) I (integral f 0.0000001)]
  (println (I 1))
  (println (I 2))
  (println (I 5))
  (println (I 3))
)
