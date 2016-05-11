(ns clojure-lectures.clara-rules-complex
  (:require [midje.sweet :refer :all]
            [clojure.test :refer :all]
            [clara.rules :refer :all]
            [clj-time.core :as t]
            [clara.rules.accumulators :as acc]
            [clara.tools.inspect]
            [clojure.string :as str])
  (:import (org.joda.time DateTime)))


;; Input facts
(defrecord GlobalSlaveDockerImage [tag])
(defrecord Request [slave-id])
(defrecord Slave [id created-at master-id status docker-image])
(defrecord Master [id office-hours pool-capacity slave-docker-image])
(defrecord NowTime [now-time])
;; Output facts
(defrecord MasterOutsideOfficeHours [master-id])
(defrecord UnusedSlave [slave-id])
(defrecord AliveSlave [slave-id])
(defrecord EffectiveSlaveDockerImage [master-id tag])
(defrecord ImageMatchingSlave [slave-id])
(defrecord ImageNotMatchingSlave [slave-id])
(defrecord SlaveToShutDown [slave-id])

(defquery query-ids-of-unused-slaves []
  [:exists [UnusedSlave (= ?id slave-id)]])

(defquery query-ids-of-alive-slaves []
  [:exists [AliveSlave (= ?id slave-id)]])

(defquery query-effective-slave-docker-images []
  [:exists [EffectiveSlaveDockerImage (= ?master-id master-id) (= ?tag tag)]])

(defquery query-ids-of-image-matching-slaves []
  [:exists [ImageMatchingSlave (= ?id slave-id)]])

(defquery query-ids-of-image-not-matching-slaves []
  [:exists [ImageNotMatchingSlave (= ?id slave-id)]])

(defquery query-ids-of-masters-outside-office-hours []
  [:exists [MasterOutsideOfficeHours (= ?id master-id)]])

(defrule unused-slave
  "Slave is unused when it is not mentioned in any active request."
  [?slave-ids-used <- (acc/distinct :slave-id) :from [Request]]
  [Slave (= ?slave-id id) (not (contains? (set ?slave-ids-used) id))]
  =>
  (insert! (->UnusedSlave ?slave-id)))

(defrule alive-slave
  "Slave is alive when it has status of CREATE_COMPLETE or CREATE_IN_PROGRESS"
  [Slave (= ?slave-id id) (#{"CREATE_COMPLETE" "CREATE_IN_PROGRESS"} status)]
  =>
  (insert! (->AliveSlave ?slave-id)))

(defn normalize-string [s]
  (if (str/blank? s)
    nil
    s))

(defn deduce-slave-docker-image [from-master from-global-config]
  (or (normalize-string from-master) (normalize-string from-global-config)))

(defrule effective-slave-docker-image
  "Effective docker image for a slave is the one that's specified in the master record,
   otherwise taken from the global config."
  [:exists [GlobalSlaveDockerImage (= ?global-slave-docker-image tag)]]
  [Master (= ?master-id id) (= ?master-slave-docker-image slave-docker-image)]
  =>
  (let [effective-image (deduce-slave-docker-image ?master-slave-docker-image ?global-slave-docker-image)]
    (insert! (->EffectiveSlaveDockerImage ?master-id effective-image))))

(defrule image-matching-slave
  "Slave is image matching when its docker image tag matches the current effective slave docker image."
  [:exists [EffectiveSlaveDockerImage (= ?effective-slave-docker-image tag) (= ?master-id master-id)]]
  [Slave (= ?slave-id id) (= ?docker-image docker-image) (= ?master-id master-id)]
  =>
  (if (= ?docker-image ?effective-slave-docker-image)
    (insert! (->ImageMatchingSlave ?slave-id))
    (insert! (->ImageNotMatchingSlave ?slave-id))))

(defn outside-hours?
  "Checks if `t` is between `start` and `end` LocalTimes."
  [t [start end]]
  (when (and start end)
    (if (t/before? start end)
      (not (t/within? start end t))
      (t/within? end start t))))

(defn is-weekend? [^DateTime t]
  (boolean (when t
             (#{6 7} (t/day-of-week t)))))

(defrule master-outside-office-hours-by-time
  "Master is considered outside of office hours when NowTime is outside this master's office hours."
  [?now <- (acc/max :now-time) :from [NowTime]]
  [Master (outside-hours? (.toLocalTime ?now) office-hours) (= ?id id)]
  =>
  (insert! (->MasterOutsideOfficeHours ?id)))

(defrule master-outside-office-on-weekend
  "Master is considered outside of office hours when NowTime is on weekend."
  [?now <- (acc/max :now-time) :from [NowTime]]
  [Master (is-weekend? ?now) (= ?id id)]
  =>
  (insert! (->MasterOutsideOfficeHours ?id)))

(defrule slave-should-be-shut-down-outside-office-hours
  "Unused slave of a master, and now is outside this master's office hours, should be shut down."
  [:exists [MasterOutsideOfficeHours (= ?master-id master-id)]]
  [:exists [UnusedSlave (= ?slave-id slave-id)]]
  [Slave (= ?slave-id id) (= ?master-id master-id)]
  =>
  (insert! (->SlaveToShutDown ?slave-id)))

(defrule slave-should-be-shut-down-when-it-exceeds-pool-size
  "Unused slave that exceeds its master's pool size and was started before others, should be shut down."
  [Master (= ?master-id id) (= ?pool-capacity pool-capacity)]
  [:test (and (number? ?pool-capacity)
              (<= 0 ?pool-capacity))]
  [?unused-slave-ids <- (acc/distinct :slave-id) :from [UnusedSlave]]
  [?unused-slaves <- (acc/all) :from [Slave (contains? ?unused-slave-ids id) (= ?master-id master-id)]]
  =>
  (doseq [{:keys [id]} (drop ?pool-capacity (reverse (sort-by :created-at ?unused-slaves)))]
    (insert! (->SlaveToShutDown id))))

(defrule slave-should-be-shut-down-when-its-image-is-not-matching
  "When the slave is based on the slave image that does not match the current effective setting for this master,
   it should be shut down."
  [:exists [ImageNotMatchingSlave (= ?slave-id slave-id)]]
  [:exists [UnusedSlave (= ?slave-id slave-id)]]
  [Slave (= ?slave-id id)]
  =>
  (insert! (->SlaveToShutDown ?slave-id)))

;; All the conditions should be true
(defquery query-ids-of-available-slaves []
  [:exists [UnusedSlave (= ?id slave-id)]]
  [:exists [AliveSlave (= ?id slave-id)]]
  [:exists [ImageMatchingSlave (= ?id slave-id)]])

;; At least one of the conditions, leading to SlaveToShutDown, should be true
(defquery query-ids-of-slaves-to-shut-down []
  [:exists [SlaveToShutDown (= ?id slave-id)]])

(defn make-session []
  (mk-session (ns-name *ns*)))

(comment
  (let [sess (-> (make-session)
                 (insert-all [(->Request "foo1")
                              (->Request "foo2")])
                 (insert-all [(map->Slave {:id "foo1"})
                              (map->Slave {:id "foo3"})])
                 (fire-rules))]
    (clara.tools.inspect/explain-activations sess)
    (prn (query sess query-ids-of-available-slaves))
    (prn (query sess query-ids-of-slaves-to-shut-down)))
  )
