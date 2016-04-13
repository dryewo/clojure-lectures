(ns clojure-lectures.async-multi
  (:require [clojure.core.async :as a :refer [chan thread >!! <!! go <! >! go-loop pipe]]))

;; Writes n strings to the channel and closes it
(defn producer [name ch n]
  (a/onto-chan ch (map #(str name %) (range n))))

(comment
  ;; Create a channel, put an collection through it
  (let [ch (chan)]
    (producer "a" ch 2)
    (println [(<!! ch) (<!! ch) (<!! ch)]))
  )

(def consumer-agent (agent nil))

;; Reads from the channel and prints it, annotated
(defn consumer [name in]
  (a/reduce (fn [_ x]
              (send consumer-agent (fn [_] (println x "->" name))))
            nil in))

(comment
  (let [ch (chan)]
    (producer "a" ch 5)
    (consumer "A" ch))

  (let [ch (chan)]
    (producer "a" ch 10)
    (consumer "A" ch)
    (consumer "B" ch))
  )

;; Returns one channel, that would be read and values written to one of the provided channels
(defn unmerge [outs]
  (let [in (chan)]
    (go-loop []
      (let [v (<! in)]
        (when-not (nil? v)
          (let [[res _] (a/alts! (for [ch outs] [ch v]))]
            (when-not res
              (a/close! in))
            (recur)))))
    in))

(defn one->one []
  (let [ch (chan)]
    (producer "a" ch 10)
    (consumer "A" ch)))

(defn many->one []
  (let [ch (chan)]
    (producer "a" ch 5)
    (producer "b" ch 5)
    (consumer "A" ch)))

(defn one->many []
  (let [ch (chan)]
    (producer "a" ch 10)
    (consumer "A" ch)
    (consumer "B" ch)))

(defn many=>one []
  (let [[ch-a ch-b] (repeatedly chan)]
    (producer "a" ch-a 5)
    (producer "b" ch-b 5)
    (consumer "A" (a/merge [ch-a ch-b]))))

(defn one=>many []
  (let [[ch-A ch-B] (repeatedly chan)]
    (producer "a" (unmerge [ch-A ch-B]) 10)
    (consumer "A" ch-A)
    (consumer "B" ch-B)))

(comment

  (do
    (one->one)
    (many->one)
    (one->many)
    (many=>one)
    (one=>many)
    )

  )
