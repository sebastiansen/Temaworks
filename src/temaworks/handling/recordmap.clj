(ns temaworks.handling.recordmap
  (:use 
    clojureql.core
    clojureql.internal
    clojureql.predicates
    temaworks.handling.prueba
    temaworks.handling.aspect
    temaworks.meta.types)
  (:import [temaworks.meta.types Entity-type Attribute Interval Reference Relationship])
  (:require [temaworks.handling.db :as db])
  (:refer-clojure
   :exclude [compile distinct drop group-by take sort conj! disj!]))

(defrecord Interval-map
  [from to])

(defrecord Many-ref-seq
  [entity-type record-maps])

(defrecord Record-map
  [#^Entity-type entity-type #^clojure.lang.IPersistentMap values])

;; values -> keys:    Attribute     | Reference  | Many-ref     | Interval
;;           vals: java.lang.Object | Record-map | Many-ref-seq | Interval-map

(defn record-map
  ([entity-type]
    (Record-map. entity-type {}))
  ([entity-type values]
    (Record-map. entity-type values))
  ([entity-type tag children]
    (Record-map. entity-type {tag children})))

(defn assoc-val
  [{:keys [entity-type values]} att-ref value]
  (Record-map. entity-type (assoc values att-ref value)))

(defn children
  [record-map]
  (:values record-map))

(defn tag [node] (key node))

(defn leaf? [node] (or 
                     (is-type? (tag node) Attribute) 
                     (is-type? (tag node) Interval)))
(defn inner? [node] (is-type? (tag node) Reference))

(defn record-map->table
  ([record-map]
    (table db/db (:table-name (:entity-type record-map)))))

(defn select-pks [record-map]
  (filter-keys (:values record-map)
    #(or (and (is-type? % Attribute) (:pk? %)) 
       (and (is-type? % Reference) (:pk? ((:rel %)))))))

(defn is?
  [this that]
  (= (select-pks (children this)) (select-pks (children that))))

(defn record-map->hack-map
  [record-map]
  (defn- helper
    [values]
    (reduce 
      (fn [total [att-ref value]]
        (merge total
          (case (type att-ref) 
            Attribute
            (hash-map (:col-name att-ref) att-ref)
            Reference
            (let [rel ((:rel att-ref))
                  mapping (clojure.set/map-invert (:fks-pks rel))
                  foreign (helper (select-pks value))]
              (reduce merge
                {}
                (map (fn [[k v]]
                       (hash-map 
                         (get mapping k)
                         v))
                  foreign))))))
      {}
      values))
  
  (helper (children record-map)))

(defn hack-map->record-map
  [entity-type hack-map]
  
  (defn- select-reference
    [hack-map reference]
    (reduce merge
      (for [[fk pk] (:fks-pks ((:rel reference)))]
        (if (contains? hack-map fk)
          (hash-map pk (get hack-map fk))
          {}))))
  
  (defn- helper [entity-type hack-map]
    (let [att-mapping (reduce merge 
                        {}
                        (map #(hash-map (:col-name %) %)
                          (:atts entity-type)))]
    (merge 
      (reduce merge
        (map #(hash-map (att-mapping (key %)) (val %))
          (filter #(contains? att-mapping (key %)) hack-map)))
      (reduce merge
        (map #(let [ref-map (select-reference hack-map %)
                    entity-type ((:to-entity ((:rel %))))]
                (if (empty? ref-map)
                  {}
                  (hash-map % (record-map
                                entity-type
                                (helper 
                                  entity-type
                                  ref-map)))))
          (:refs entity-type))))))
  (record-map entity-type (helper entity-type hack-map)))