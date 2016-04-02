(ns clojure-lectures.transducers
  (:require [midje.sweet :refer :all]))

;; Debug print utils

(defn spyfn
  ([f name]
   (fn [& args]
     (let [result   (apply f args)
           ;args-str (clojure.string/join " " (cons name (map pr-str (vec args))))
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

(comment
  ((spy (comp inc inc)) 1)
  ((spy (fn [x] (str x))) 1))

(defn flip [f]
  (fn [a1 a2 & args]
    (apply f a2 a1 args)))

(comment
  (reduce-kv #(do (prn %1 %2 %3 %&) %1) 42 {:a 1 :b 2})
  )

;; https://codepad.remoteinterview.io/

;; 0. Recap

(comment
  (identity {:a 11})
  (conj [1 2] 3)
  (conj '(1 2) 3)
  (reduce + [1 2 3])
  (map inc [1 2 3])
  (map str [1 2 3])
  )


;; 1. Implement identity-coll via reduce

(defn identity-coll-via-reduce [coll]
  (reduce conj [] coll))
(defn identity-coll-via-reduce [coll]
  (reduce conj [] coll))

(facts "identity-coll-via-reduce"
  (identity-coll-via-reduce [1 2 3]) => [1 2 3])


;; 2. Implement map via reduce

(defn map-via-reduce [f coll]
  (reduce (fn [acc x] (conj acc (f x))) [] coll))
(defn map-via-reduce [f coll]
  (reduce (fn [acc x]
            (conj acc (f x)))
          [] coll))

(facts "map-via-reduce"
  (map-via-reduce inc [1 2 3]) => [2 3 4])


;; 3. Think of examples of reducing functions
;; (Acc, X) => Acc

;; conj
;; +
;; -
;; comp
;; str
;; flipped cons
;; and
;; or
;; bitwise ops

(comment
  (reduce (spy +) 100 [1 2 3])
  (reduce (spy *) 100 [1 2 3])
  (reduce (spy conj) [100] [1 2 3])
  (reduce (spy str) "100" [1 2 3])
  (reduce (spy max) Integer/MIN_VALUE [1 3 2])
  (reduce (spy (flip cons)) '(100) [1 2 3])
  ((reduce (spy comp) [str inc inc inc inc inc])
    100)
  ((comp str inc inc inc inc inc) 100)
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

(def map-inc (map (spy inc)))
(def map-str (map (spy str)))
(def filter-odd (filter (spy odd?)))

(comment
  (reduce (spy +) 100 [1 2 3])
  (reduce (map-inc (spy +)) 100 [1 2 3])

  (reduce + 100 (filter odd? [1 2 3]))
  (reduce (filter-odd (spy +)) 100 [1 2 3])

  (reduce (spy conj) [100] [1 2 3])

  (reduce conj [100] (map inc [1 2 3]))
  (reduce (map-inc (spy conj)) [100] [1 2 3])
  (reduce (filter-odd (spy conj)) [100] [1 2 3])

  (map (comp inc inc inc) [1 2 3])
  (map inc (map inc (map inc [1 2 3])))

  ;; reduce on steroids
  (transduce map-inc (spy +) [1 2 3])

  (reduce + 0 (filter odd? [1 2 3]))
  (transduce (filter odd?) + [1 2 3])

  (reduce + 0 (filter odd? (map inc [1 2 3])))
  (transduce (comp (map inc) (filter odd?)) + [1 2 3])

  ;; no initial value, broken
  (reduce (map-inc (spy +)) [1 2 3])

  (into [100] [1 2 3 4])
  (into '(100) [1 2 3 4])
  (into #{100} [1 2 3 4])

  (let [trx (map inc)]
    (into [100] trx [1 2 3]))
  (into [100] (filter odd?) [1 2 3])
  ;; same as
  (transduce map-inc (spy conj) [100] [1 2 3])
  (reduce (map-inc (spy conj)) [100] [1 2 3])

  ;; transform multiple collections at once
  (sequence map-inc [1 2 3])
  ;; same as
  (transduce map-inc (spy conj) [100] [1 2 3])

  ;; but can be used with more than 1 coll
  (map + [1 2 3] [4 5] [6 7])
  (sequence (map (spy +)) [1 2 3] [4 5] [6 7])

  ;; Bundle transducer with collection
  (let [ed (eduction map-inc (range 3))]
    (prn "Hello")
    (prn (first ed))
    (reduce (spy str) "===" ed))
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
(defn make-identity-transducer []
  (fn [rf]
    (fn
      ([] (rf))
      ([acc] (rf acc))
      ([acc x] (rf acc x)))))

(facts "identity-transducer"
  (transduce (make-identity-transducer) + [1 2 3]) => (transduce identity + [1 2 3])
  (transduce (make-identity-transducer) conj [1 2 3]) => (transduce identity conj [1 2 3]))


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
  (->> (range 40)
       (map (spy inc))
       (filter (spy odd?)))
  (sequence (comp map-inc filter-odd) (range 40))

  (->> (range 40)
       (filter (spy odd?))
       (map (spy inc)))
  (sequence (comp filter-odd map-inc) (range 40))

  (let [coll (vec (range 1000000))]
    (time (count (sequence (comp (map inc) (map inc) (map inc) (map inc) (map str)) coll)))
    (time (count (->> coll (map inc) (map inc) (map inc) (map inc) (map str)))))
  )


;; 8. Replacing transducer

(def map-dec (map (spy dec)))

(comment
  (transduce (comp map-dec
                   (constantly (spy +))
                   map-inc)
             (spy *)
             [1 2 3])
  (transduce (comp map-dec
                   (constantly (spy str))
                   map-inc)
             (spy conj)
             [1 2 3])
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
    (time (count (sequence (distinct) coll)))
    (time (count (sequence (my-dedupe) coll))))
  )

(facts "my-dedupe"
  (sequence (my-dedupe) [1 1 2 2 1 1]) => (sequence (dedupe) [1 1 2 2 1 1]))
