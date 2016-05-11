(ns clojure-lectures.clara-rules-simple
  (:require [midje.sweet :refer :all]
            [clojure.test :refer :all]
            [clara.rules :refer :all]
            [clara.tools.inspect]))

;; Input facts
(defrecord Rent [id price])
(defrecord RentThreshold [price])
(defrecord CantAffordBreakfastLunchAndDinner [rent-id])
;; Output facts
(defrecord RentIsTooDamnHigh [rent-id])
(defrecord RentShouldBeLowered [rent-id amount])

(defquery query-high-rents []
  [RentIsTooDamnHigh (= ?rent-id rent-id)])

(defquery query-rents-to-lower []
  [RentShouldBeLowered (= ?rent-id rent-id) (= ?amount amount)])

(defrule rent-is-too-damn-high
  "Rent is too damn high when it is higher than the threshold"
  [RentThreshold (= ?threshold-price price)]
  [Rent (= ?id id) (> price ?threshold-price)]
  =>
  (insert! (->RentIsTooDamnHigh ?id)))

(defrule rent-should-be-lowered
  "Rent should be lowered when it's too damn high and you can't afford breakfast, lunch and dinner"
  [RentIsTooDamnHigh (= ?rent-id rent-id)]
  [RentThreshold (= ?threshold-price price)]
  [CantAffordBreakfastLunchAndDinner (= ?rent-id rent-id)]
  [Rent (= ?rent-id id) (= ?price price)]
  =>
  (insert! (->RentShouldBeLowered ?rent-id (- ?threshold-price ?price))))

;(defrule bogus
;  [RentIsTooDamnHigh (= ?rent-id rent-id)]
;  =>
;  (insert! (->Rent ?rent-id 55555)))

(defn make-session []
  (mk-session (ns-name *ns*)))

(comment
  (let [sess (-> (make-session)
                 (insert-all [(->Rent 1 8000)
                              (->Rent 2 9500)
                              (->Rent 3 10000)])
                 (insert-all [(->RentThreshold 9000)])
                 (insert-all [(->CantAffordBreakfastLunchAndDinner 2)])
                 (fire-rules))]
    (clara.tools.inspect/explain-activations sess)
    (println "Too Damn High:" (query sess query-high-rents))
    (println "Should be lowered:" (query sess query-rents-to-lower)))
  )
