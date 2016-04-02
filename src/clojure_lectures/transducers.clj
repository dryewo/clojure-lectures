(ns clojure-lectures.transducers
  (:require [midje.sweet :refer :all]))

;; Debug print utils

(defn spyfn
  ([f name]
   (fn [& args]
     (let [result   (apply f args)
           args-str (->> (vec args)
                         (map pr-str)
                         (cons name)
                         (clojure.string/join " "))
           ]
       (println (format "(%s) => %s" args-str (pr-str result)))
       result))))

(defmacro spy [f]
  (let [fname (str f)]
    `(spyfn ~f ~fname)))

(defmacro with-spy [fns & body]
  (let [bindings (vec (mapcat #(vector % `(spy ~%)) fns))]
    `(let ~bindings
       ~@body)))

(comment
  ((spy (comp inc inc)) 1)
  ((spy (fn [x] (str x))) 1)
  (macroexpand-1 `(with-spy [+] (+ 1 2))))

(defn flip [f]
  (fn [a1 a2 & args]
    (apply f a2 a1 args)))


;; 0. Recap

(comment
  (identity [])
  (conj [1 2] 3)
  (reduce + 0 [1 2 3])
  (map inc [1 2 3])
  (map str [1 2 3])
  )


;; 1. Implement identity-coll via reduce

(defn identity-coll-via-reduce [coll]
  (reduce conj [] coll))

(facts "identity-coll-via-reduce"
  (identity-coll-via-reduce [1 2 3]) => [1 2 3])


;; 2. Implement map via reduce

(defn map-via-reduce [f coll]
  (reduce (fn [acc x] (conj acc (f x))) [] coll))

(facts "map-via-reduce"
  (map-via-reduce inc [1 2 3]) => [2 3 4])


;; 3. Think of examples of reducing functions
;; (Acc, X) => Acc

(comment
  (with-spy [+]
    (reduce + 100 [1 2 3]))
  (with-spy [*]
    (reduce * 100 [1 2 3]))
  (with-spy [conj]
    (reduce conj [100] [1 2 3]))
  (with-spy [str]
    (reduce str "100" [1 2 3]))
  (with-spy [max]
    (reduce max Integer/MIN_VALUE [1 3 2]))
  (let [prepend (flip cons)]
    (with-spy [prepend]
      (reduce prepend '(100) [1 2 3])))
  (with-spy [comp]
    ((reduce comp [str inc inc inc inc inc]) 100))
  )


;; Recap: Higher-order functions
(facts ""
  ((complement odd?) 3) => (even? 3)
  )


;; Transducer: ((Accx, X) => Accx) => ((Accy, Y) => Accy)
;; Transducer may be implemented as middleware (aka decorator)

;; Simplest transducer?

;; How to obtain?

(comment
  (map inc)
  (filter odd?)
  (remove even?)
  (take 5)
  (take-while neg?)
  (drop 42)
  (drop-while nil?)
  (partition-all 2)
  (partition-by pos?)
  (dedupe)
  (distinct)
  (random-sample 0.5)
  cat

  ((map inc) +)
  ((map inc) ((map inc) conj))
  )


;; Use cases:

;(def map-inc (map (spy inc)))
;(def map-str (map (spy str)))
;(def filter-odd (filter (spy odd?)))

(comment
  (with-spy [+]
    (reduce + 100 [1 2 3]))
  (with-spy [+ inc]
    (reduce ((map inc) +) 100 [1 2 3]))

  (with-spy [+ odd?]
    (reduce + 100 (filter odd? [1 2 3])))
  (with-spy [+ odd?]
    (reduce ((filter odd?) +) 100 [1 2 3]))

  (with-spy [conj]
    (reduce (spy conj) [100] [1 2 3]))

  (with-spy [conj]
    (reduce conj [100] (map inc [1 2 3])))
  (with-spy [conj inc]
    (reduce ((map inc) conj) [100] [1 2 3]))
  (with-spy [conj odd?]
    (reduce ((filter odd?) conj) [100] [1 2 3]))

  (with-spy [inc]
    (map (comp inc inc inc) [1 2 3]))
  (with-spy [inc]
    (map inc (map inc (map inc [1 2 3]))))

  ;; reduce on steroids
  (with-spy [+ inc]
    (transduce (map inc) + [1 2 3]))

  (with-spy [+ odd?]
    (reduce + 0 (filter odd? [1 2 3])))
  (with-spy [+ odd?]
    (transduce (filter odd?) + [1 2 3]))

  (with-spy [+ odd?]
    (reduce + 0 (filter odd? (map inc [1 2 3]))))
  (with-spy [+ odd? inc]
    (transduce (comp (map inc) (filter odd?)) + [1 2 3]))

  ;; no initial value, broken
  (with-spy [+ inc]
    (reduce ((map inc) +) [1 2 3]))

  (into [100] [1 2 3 4])
  ;; same as
  (with-spy [inc conj]
    (transduce (map inc) conj [100] [1 2 3]))
  (with-spy [inc conj]
    (reduce (map-inc (spy conj)) [100] [1 2 3]))

  ;; also
  (let [trx (map inc)]
    (into [100] trx [1 2 3]))
  (into [100] (filter odd?) [1 2 3])
  (into '(100) [1 2 3 4])
  (into #{100} [1 2 3 4])

  ;; transform multiple collections at once
  (with-spy [inc conj]
    (sequence (map inc) [1 2 3]))
  ;; same as
  (with-spy [inc conj]
    (transduce (map inc) conj [100] [1 2 3]))

  ;; but can be used with more than 1 coll
  (with-spy [+]
    (sequence (map +) [1 2 3] [4 5] [6 7]))
  (with-spy [+]
    (map + [1 2 3] [4 5] [6 7]))

  ;; Bundle transducer with collection
  (with-spy [inc str]
    (let [ed (eduction (map inc) (range 3))]
      (prn "Hello")
      (prn (first ed))
      (reduce str "===" ed)))
  )


;; Recap: Multi-arity functions
(def multi-arity-fn
  (fn
    ([] (println "Arity 0"))
    ([x] (println "1 arg:" x))
    ([x y] (println "Called with" x "and" y))))


;; 4. Implement an identity transducer

(defn identity-transducer [rf]
  (fn
    ([] (rf))
    ([result] (rf result))
    ([result input] (rf result input))))

(facts "identity-transducer"
  (transduce identity-transducer + [1 2 3]) => (transduce identity + [1 2 3])
  (transduce identity-transducer conj [1 2 3]) => (transduce identity conj [1 2 3]))


;; 5. Implement (map f)

(defn make-map-transducer [f]
  (fn [rf]
    (fn
      ([] (rf))
      ([acc] (rf acc))
      ([acc x] (rf acc (f x))))))

(facts "make-map-transducer"
  (transduce (make-map-transducer inc) + [1 2 3]) => (transduce (map inc) + [1 2 3])
  (transduce (make-map-transducer inc) conj [1 2 3]) => (transduce (map inc) conj [1 2 3]))


;; 6. Implement (filter pred)

(defn make-filter-transducer [pred]
  (fn [rf]
    (fn
      ([] (rf))
      ([acc] (rf acc))
      ([acc x] (if (pred x)
                 (rf acc x)
                 acc)))))

(facts "make-filter-transducer"
  (transduce (make-filter-transducer odd?) + [1 2 3]) => (transduce (filter odd?) + [1 2 3])
  (transduce (make-filter-transducer odd?) conj [1 2 3]) => (transduce (filter odd?) conj [1 2 3]))


;; Composing transducers

(comment
  (with-spy [inc odd?]
    (->> (range 40)
         (map inc)
         (filter odd?)))
  (with-spy [inc odd?]
    (sequence (comp (map inc) (filter odd?)) (range 40)))

  (with-spy [inc odd?]
    (->> (range 40)
         (filter odd?)
         (map inc)))
  (with-spy [inc odd?]
    (sequence (comp (filter odd?) (map inc)) (range 40)))

  (let [coll (vec (range 1000000))]
    (time (count (sequence (comp (map inc) (map inc) (map inc) (map inc) (map str)) coll)))
    (time (count (->> coll (map inc) (map inc) (map inc) (map inc) (map str)))))
  )


;; 8. Replacing transducer

(comment
  (with-spy [dec inc + *]
    (transduce (comp (map dec)
                     (constantly +)
                     (map inc))
               *
               [1 2 3]))
  (with-spy [dec inc str conj]
    (transduce (comp (map dec)
                     (constantly str)
                     (map inc))
               conj
               [1 2 3]))
  )


;; 9. Stateful transducers

(comment
  (sequence (dedupe) [1 1 2 2 1 1])
  (sequence (distinct) [1 1 2 2 1 1])

  (sequence (take 2) [1 2 3])
  (sequence (drop 2) [1 2 3])
  )


;; Recap: Closures
(let [counter (atom 0)]
  (defn next-counter []
    (swap! counter inc)))

(comment
  (next-counter)
  )


;; 10. Implement (dedupe)

(defn my-dedupe []
  (fn [rf]
    (let [prev (atom ::none)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (if (= @prev input)
           result
           (do
             (reset! prev input)
             (rf result input))))))))

(comment
  (let [coll (vec (range 1000000))]
    (time (count (sequence (dedupe) coll)))
    (time (count (sequence (my-dedupe) coll))))
  )

(facts "my-dedupe"
  (sequence (my-dedupe) [1 1 2 2 1 1]) => (sequence (dedupe) [1 1 2 2 1 1]))
