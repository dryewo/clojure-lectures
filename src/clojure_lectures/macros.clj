(ns clojure-lectures.macros
  (:require [midje.sweet :refer :all]))

(defn spyfn
  ([f name]
   (fn [& args]
     (let [result   (apply f args)
           args-str (->> (vec args)
                         (map pr-str)
                         (cons name)
                         (clojure.string/join " "))]
       (println
         (format "(%s) => %s" args-str (pr-str result)))
       result))))

(defmacro spy [f]
  (let [fname (str f)]
    `(spyfn ~f ~fname)))

(defmacro with-spy [fns & body]
  (let [bindings (vec (mapcat #(vector % `(spy ~%)) fns))]
    `(let ~bindings
       ~@body)))


;; What are we going to build?

(comment
  (reduce + [1 2 3 4 5 6])

  (reduce (spyfn + "+") [1 2 3 4 5 6])                      ;; not a macro

  (reduce (spy *) [1 2 3 4 5 6])
  (reduce (spy (fn [x y] (+ x y))) [1 2 3 4 5 6])

  (with-spy [+]
    (reduce + [1 2 3 4 5 6]))

  (reduce + [1 2 3 4 5 6])

  (with-spy [+]
    (transduce (map inc) + [1 2 3 4 5 6]))
  (with-spy [+ inc]
    (transduce (map inc) + [1 2 3 4 5 6]))
  )

;; Recap: macros are functions

(defmacro hello-macro []
  (prn "Hello, macro"))

(comment
  (hello-macro)
  )

;; Macros is called when the definition is evaluated

(defn fun1 []
  (prn "fun1 called")
  (hello-macro)
  1)

;; Not when the actual function is called

(comment
  (fun1)
  )

;; Debugging macros: macroexpand

(comment
  (macroexpand (hello-macro))

  (macroexpand '(spy +))
  (macroexpand '(spy (fn [x y] (+ x y))))
  (macroexpand '(with-spy [+] (reduce + [1 2 3 4 5 6])))

  (macroexpand '(hello-macro))
  )

;; Evaluation and quoting

(comment
  (spy +)

  '(spy +)
  (list 'spy '+)
  ('spy '+)
  ('spy {'spy 42})

  (first '(spy +))
  (next '(spy +))
  )

;; Syntax quoting

(comment
  '+
  `+
  (name `+)
  (namespace +)
  (name :foo/bar)
  (namespace :foo/bar)

  'foo'bar
  `++++++
  '(+ (-))
  `(+ (- (foo)))
  `(++)
  )

;; Let's try

(defmacro spy1 [f]
  (println f)
  (list 'spyfn f (str f)))

(facts "about spy1"
  (macroexpand '(spy1 +)) => (list 'spyfn '+ "+")
  (macroexpand '(spy1 -)) => (list 'spyfn '- "-"))

;; Now with syntax-quote

(comment
  (spy1 (fn [x y] (* x y)))
  (macroexpand '(spy1 *))
  (str '+ '-)
  )

(defmacro spy2 [f]
  (list `spyfn f (str f)))

(defmacro spy2 [f]
  `(spyfn ~f ~(str f)))

(facts "about spy2"
  (macroexpand '(spy2 +)) => '(clojure-lectures.macros/spyfn + "+")
  (macroexpand '(spy2 +)) => (list `spyfn '+ "+"))

;; with-spy

(comment
  (let [+   (spy +)
        inc (spy inc)]
    (+ 3 (inc 5)))

  (with-spy [+ inc]
    (+ 3 (inc 5)))
  )

(facts "about with-spy"
  (macroexpand '(with-spy [+] (+ 3 5)))
  =>
  '(let* [+ (clojure-lectures.macros/spy +)] (+ 3 5)))

(defmacro myform [] (prn &env))

(comment
  (let [a 1]
    (myform)))
