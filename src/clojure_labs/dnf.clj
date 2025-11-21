(ns clojure-labs.dnf)

;; -----------------------------
;;  Helpers: core AST primitives

(defn ast? "check if ast-tag in expr is equal to a given tag" [expr tag]
  (= (first expr) tag)
)

(defn make-ast "create an AST entry (operation or variable)" [tag & args]
  (apply list tag args))

(defn args "get AST entry args: (tag expr)" [expr]
  (rest expr))

;; -----------------------------
;;  Core entities & helpers

(defn variable "create a variable named name" [name]
  {:pre [(keyword? name)]}
  (make-ast :var name))

(defn variable? "check if given expr is a variable" [expr]
  (ast? expr :var))

(defn variable-name "get variable name" [expr]
  (second expr))

(def const-true "true constant value" (list :true))
(def const-false "false constant value" (list :false))

(defn const-true? "check if given expr is const-true" [expr] (ast? expr :true))
(defn const-false? "check if given expr is const-false" [expr] (ast? expr :false))

(defn const? "check if given expr is const-false or const-true" [expr]
  (or (const-true? expr)
      (const-false? expr)
  )
)

;; -----------------------------
;;  Logical operations

(defn negation "operation NOT" [& exprs]
  (apply make-ast :negation exprs))

(defn negation? "check if given expr is the operation NOT" [expr]
  (ast? expr :negation))


(defn conjunction "operation AND" [& exprs]
  (apply make-ast :conjunction exprs))

(defn conjunction? "check if given expr is the operation AND" [expr]
  (ast? expr :conjunction))


(defn disjunction "operation OR" [& exprs]
  (apply make-ast :disjunction exprs))

(defn disjunction? "check if given expr is the operation OR" [expr]
  (ast? expr :disjunction))


(defn implication "operation of implication ( -> )" [& exprs]
  (apply make-ast :implication exprs))

(defn implication? "check if given expr is implication" [expr]
  (ast? expr :implication))

(defn pierce "Pierce's arrow operation ( NOR )" [& exprs]
  (apply make-ast :pierce exprs))

(defn pierce? "check if given expr is implication" [expr]
  (ast? expr :pierce))

;; -----------------------------
;;  DNF transformation

(defn to-dnf-by-step [expr rules]
  (let [[_ transform]
        (some (fn [[pred f :as pair]]
                (when (pred expr) pair)
              )
          rules
        )
      ]
    (transform expr)
  )
)

;; =========================================================
;; define-expr*: replace vars with their values from the map
;; subst-map = { :A const-true, :B const-false, ... }
(declare define-expr*)

(defn define-rules*
  [subst-map]
  (list

    ;; if expr — var and exists in subst-map
    [
       (fn [expr]
         (and (variable? expr) (contains? subst-map (variable-name expr)))
       )
       (fn [expr]
         (get subst-map (variable-name expr))
       )
    ]

    ;; do nothing with other vars and consts
    [
       (fn [expr] (or (variable? expr) (const? expr)))
       (fn [expr] expr)
    ]

    ;; NOT
    [
       (fn [expr] (negation? expr))
       (fn [expr]
         (negation (define-expr* subst-map (second expr)))
       )
    ]

    ;; AND
    [
       (fn [expr] (conjunction? expr))
       (fn [expr]
         (apply conjunction
                (map #(define-expr* subst-map %) (args expr))
         )
       )
    ]

    ;; OR
    [
       (fn [expr] (disjunction? expr))
       (fn [expr]
         (apply disjunction
                (map #(define-expr* subst-map %) (args expr))
         )
       )
    ]

  )
)

;; apply 1st step rules on expression
(defn define-expr* [subst-map expr]
  (to-dnf-by-step expr (define-rules* subst-map))
)

;; =========================================================
;;  Step 1: replace complex operations with AND, OR, NOT
(declare step1-expr)

(def step1-rules
  (list

    ;; apply NOT recursively
    [
       (fn [expr] (negation? expr))
       (fn [expr]
          (negation (step1-expr (second expr)))
       )
    ]

    ;;  apply AND operation on all args
    [
       (fn [expr] (conjunction? expr))
       (fn [expr]
         (apply conjunction (map #(step1-expr %) (args expr)))
       )
    ]

    ;; apply OR operation on all args
    [
       (fn [expr] (disjunction? expr))
       (fn [expr]
         (apply disjunction (map #(step1-expr %) (args expr)))
       )
    ]

    ;; A -> B === NOT A OR B
    [
       (fn [expr] (implication? expr))
       (fn [expr]
         (step1-expr                            ;; apply step1 recursively
           (disjunction
             (negation (step1-expr (first (args expr)))) ;; NOT step1(A)
             (step1-expr (second (args expr)))           ;; B step1(B)
           )
         )
       )
    ]

    ;; Pierce's arrow with A, B === NOT (A OR B)
    [
       (fn [expr] (pierce? expr))
       (fn [expr]
         (negation (apply disjunction (map step1-expr (args expr))))
       )
    ]

    ;; do nothing with vars and consts
    [
     (fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)
    ]
  )
)

;; apply 2nd step rules on expression
(defn step1-expr [expr]
  (to-dnf-by-step expr step1-rules))

;; =========================================================
;; Step 2: apply de Morgan's laws
(declare step2-expr)

(def step2-rules
  (list

    ;; NOT (A AND B) === NOT A OR NOT B
    [
       (fn [expr] (and (negation? expr) (conjunction? (second expr))))
       (fn [expr]
         (step2-expr
           (apply disjunction
                  (map #(negation %) (args (second expr)))
           )
         )
       )
    ]

    ;; NOT (A OR B) === NOT A AND NOT B
    [
       (fn [expr] (and (negation? expr) (disjunction? (second expr))))
       (fn [expr]
         (step2-expr
           (apply conjunction
                  (map #(negation %) (args (second expr)))
           )
         )
       )
    ]

    [
     (fn [expr] (negation? expr))
     (fn [expr]
       (negation (step2-expr (second expr)))
     )
    ]

    [
     (fn [expr] (conjunction? expr))
     (fn [expr]
       (apply conjunction (map step2-expr (args expr)))
     )
    ]

    [
     (fn [expr] (disjunction? expr))
     (fn [expr]
       (apply disjunction (map step2-expr (args expr)))
     )
    ]

    [
     (fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)
    ]
  )
)

(defn step2-expr [expr]
  (to-dnf-by-step expr step2-rules))

;; =========================================================
;; Step 3: simplification of NOT's and consts
(declare step3-expr)

(def step3-rules
  (list

    ;; NOT NOT A === A
    [
     (fn [expr] (and (negation? expr) (negation? (second expr))))
     (fn [expr] (step3-expr (first (args (second expr)))))
    ]

    ;; NOT true === false
    [
     (fn [expr] (and (negation? expr) (const-true? (first (args expr)))))
     (fn [expr] const-false)
    ]

    ;; NOT false === true
    [(fn [expr] (and (negation? expr) (const-false? (first (args expr)))))
     (fn [expr] const-true)
    ]

    ;; call NOT on expr recursively
    [(fn [expr] (negation? expr))
     (fn [expr] (negation (step3-expr (second expr))))
    ]

    ;; call AND on expr recursively
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map step3-expr (args expr))))
    ]

    ;; call OR on expr recursively
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map step3-expr (args expr))))
    ]

    ;; do nothing with consts & vars
    [(fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)
    ]
  )
)

;; apply 3d step rules on expression
(defn step3-expr [expr]
  (to-dnf-by-step expr step3-rules)
)

;; =========================================================
;; Step 4: apply distributive property
(declare step4-expr)

(def step4-rules
  (list

    ;; A AND (B OR C) === (A AND B) OR (A AND C)
    [
     (fn [expr] (and (conjunction? expr) (disjunction? (second (args expr)))))
     (fn [expr]
       (step4-expr
         (disjunction
           (conjunction (first (args expr)) (first (args (second (args expr)))))
           (conjunction (first (args expr)) (second (args (second (args expr)))))
         )
       )
     )
    ]

    ;; (A OR B) AND C === (A AND C) OR (B AND C)
    [
     (fn [expr] (and (conjunction? expr) (disjunction? (second expr))))
     (fn [expr]
       (step4-expr
         (disjunction
           (conjunction (first (args (first (args expr)))) (second (args expr)))
           (conjunction (second (args (first (args expr)))) (second (args expr)))
         )
       )
     )
    ]

    [
     (fn [expr] (negation? expr))
     (fn [expr] (negation (step4-expr (second expr))))
    ]

    [
     (fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map step4-expr (args expr))))
    ]

    [
     (fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map step4-expr (args expr))))
    ]

    [
     (fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)
    ]
  )
)

;; apply 4th step rules on expression
(defn step4-expr [expr]
  (to-dnf-by-step expr step4-rules)
)

;; =========================================================
;; Step 5: process consts in expression
(declare step5-expr)

(def step5-rules
  (list

    ;; conjunction with a const
    [
     (fn [expr] (and (conjunction? expr) (some const? (args expr))))
     (fn [expr]
       (let [vals (args expr)]
         (cond
           ;; if const is false === everything is false
           (some const-false? vals)
           const-false

           ;; if const is true === everything is true
           (empty? (filter #(not (= const-true %)) vals))
           const-true

           :else
           (apply conjunction
                  (map step5-expr (filter #(not (= const-true %)) vals))
           )
         )
       )
     )
    ]

    ;; disjunction with a const
    [
     (fn [expr] (and (disjunction? expr) (some const? (args expr))))
     (fn [expr]
       (let [vals (args expr)]
         (cond
           ;; if const is true === everything is true
           (some const-true? vals)
           const-true

           ;; if there are nothing after the false filtering === result is false
           (empty? (filter #(not (= const-false %)) vals))
           const-false

           :else
           (apply disjunction
                  (map step5-expr (filter #(not (= const-false %)) vals))
           )
         )
       )
     )
    ]

    [
     (fn [expr] (negation? expr))
     (fn [expr] (negation (step5-expr (second expr))))
    ]

    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map step5-expr (args expr))))
    ]

    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map step5-expr (args expr))))
    ]

    [(fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)
    ]
  )
)

;; call it recursively until we cant make it easier
(defn step5-expr [expr]
  (let [result (to-dnf-by-step expr step5-rules)]
    (if (= result expr)
      expr
      (step5-expr result)
    )
  )
)

;; =========================================================
;; Step 6: removing single conjunctions and disjunctions
(declare step6-expr)

(def step6-rules
  (list

    ;; conjunction with one arg
    [
     (fn [expr] (and (conjunction? expr) (= (count (args expr)) 1)))
     (fn [expr] (second expr))
    ]

    ;; conjunction with one arg
    [
     (fn [expr] (and (disjunction? expr) (= (count (args expr)) 1)))
     (fn [expr] (second expr))
    ]

    [
     (fn [expr] (negation? expr))
     (fn [expr] (negation (step6-expr (second expr))))
    ]

    [
     (fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map step6-expr (args expr))))
    ]

    [
     (fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map step6-expr (args expr))))
    ]

    [
     (fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)
    ]
  )
)

(defn step6-expr [expr]
  (to-dnf-by-step expr step6-rules)
)

;; main expr to DNF function - apply steps 1-6
(defn to-dnf
  ([expr]                                                   ;; just simplify
   (->> expr
        step1-expr
        step2-expr
        step3-expr
        step4-expr
        step5-expr
        step6-expr
        )
   )
  ([subst-map expr]                                         ;; simplify, put values from map and simplify again
    (->> expr
        step1-expr
        step2-expr
        step3-expr
        step4-expr
        step5-expr
        step6-expr
        (define-expr* (into {} (map (fn [[k v]] [(keyword k) v]) subst-map)))
        step3-expr
        step5-expr
        step6-expr
    )
  )
)

; tests

(defn -main []
  (println "true: " (to-dnf const-true))
  (println "!!true: " (to-dnf (negation (negation const-true))))
  (println "!A: " (to-dnf (negation (variable :A))))
  (println "!!A: " (to-dnf (negation (negation (variable :A)))))
  (println "A or true" (to-dnf (disjunction (variable :A) const-true)))
  (println "false and B" (to-dnf (conjunction const-false (variable :B))))
  (println "A -> B: " (to-dnf (implication (variable :A) (variable :B))))
  (println "true -> B: " (to-dnf (implication const-true (variable :B))))
  (println "A XOR B: " (to-dnf (pierce (variable :A) (variable :B))))
  (println "A XOR (B or !C): " (to-dnf (pierce (variable :A) (disjunction (variable :B) (negation (variable :C))))))
  (println "A XOR (B or ! false): " (to-dnf (pierce (variable :A) (disjunction (variable :B) (negation const-false)))))
  (println "false XOR (B or !C): " (to-dnf (pierce const-false (disjunction (variable :B) (negation (variable :C))))))
  (println "A or (B XOR !C): " (to-dnf (disjunction (variable :A) (pierce (variable :B) (negation (variable :C))))))

  (println)

  (println "!A (A = true): " (to-dnf {:A const-true} (negation (variable :A))))
  (println "!!A (A = true): " (to-dnf {:A const-true} (negation (negation (variable :A)))))
  (println "A or B (A = false): " (to-dnf {:A const-false} (disjunction (variable :A) (variable :B))))
  (println "!A and B (A = true): " (to-dnf {:A const-true} (conjunction (negation (variable :A)) (variable :B))))
  (println "A or B (A = false, B = true): " (to-dnf {:A const-false :B const-true} (disjunction (variable :A) (variable :B))))
  (println "A -> B (A = false): " (to-dnf {:A const-true} (implication (variable :A) (variable :B))))
  (println "A XOR B (A = false, B = true): " (to-dnf {:A const-false :B const-true} (pierce (variable :A) (variable :B))))
  (println "A XOR B (A = false, B = false): " (to-dnf {:A const-false :B const-false} (pierce (variable :A) (variable :B))))
  (println "A XOR (B or !C) (C = false): " (to-dnf {:C const-false} (pierce (variable :A) (disjunction (variable :B) (negation (variable :C))))))
)