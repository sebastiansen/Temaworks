(ns temaworks.handling.cloudmap
  (:use 
    clojureql.core
    clojureql.internal
    clojureql.predicates
    temaworks.handling.prueba
    temaworks.handling.aspect
    temaworks.meta.types)
  (:import [temaworks.meta.types Entity-type Attribute Reference Relationship])
  (:require [temaworks.handling.db :as db])
  (:refer-clojure
   :exclude [compile distinct drop group-by take sort conj! disj!]))

;; You have an error in your SQL syntax; check the manual that corresponds to your MySQL server version for the right syntax to use near '* FROM temaworks-user WHERE (temaworks-user.user = 'reda')' at line 1

(defn children
  [cloud-map]
  (if (map? cloud-map)
    (val (first cloud-map))
    (val cloud-map)))

(defn root
  [cloud-map]
  (if (map? cloud-map)
    (key (first cloud-map))
    (key cloud-map)))

(defn leaf? [node] (is-type? (root node) Attribute))
(defn root? [node] (is-type? (root node) Entity-type))
(defn inner? [node] (is-type? (root node) Reference))

(defn cloud-map->table
  ([cloud-map]
    (table db/db (:table-name (root cloud-map)))))

(defn assoc-ref
  [child-map reference parent-map]
  (if-not (empty? (children parent-map))
    (hash-map (root child-map) 
      (assoc (children child-map) 
        reference (children parent-map)))
    child-map))

(defn select-pks [ref-branch]
  (filter-keys (val ref-branch) 
    #(or (and (is-type? % Attribute) (:pk? %)) 
       (and (is-type? % Reference) (:pk? ((:rel %)))))))

(defn cloud-map->hack-map
  [cloud-map]
  (defn- helper
    [entity-type cloud-map]
    (reduce 
      (fn [total entry]
        (let [[att-ref value] entry]
          (merge total
            (if (is-type? att-ref Attribute)
              (hash-map (:col-name att-ref)
                value)
              (let [rel ((:rel att-ref))
                    mapping (clojure.set/map-invert (:fks-pks rel))
                    foreign (helper 
                              ((:to-entity rel)) 
                              (select-pks entry))]
                (reduce merge
                  {}
                  (map (fn [[k v]]
                         (hash-map 
                           (get mapping k)
                           v))
                    foreign)))))))
      {}
      cloud-map))
  (helper (root cloud-map) (children cloud-map)))

;;{#:temaworks.meta.types.Entity-type{:table-name :travel} 
;; {#:temaworks.meta.types.Reference{:ref-name Origen} nil,
;;  #:temaworks.meta.types.Reference{:ref-name Destino} nil, 
;;  #:temaworks.meta.types.Attribute{:att-name Date} #<Date Wed Feb 16 00:33:23 CLST 2011>}}

(defn select-reference
    [hack-map reference]
    (reduce merge
      (for [[fk pk] (:fks-pks ((:rel reference)))]
        (if (contains? hack-map fk)
          (hash-map pk (get hack-map fk))
          {}))))

(defn hack-map->cloud-map
  [entity-type hack-map]
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
        (map #(let [ref-map (select-reference hack-map %)]
                (if (empty? ref-map)
                  {}
                  (hash-map % (helper 
                                ((:to-entity ((:rel %))))
                                ref-map))))
          (:refs entity-type))))))
  (hash-map entity-type (helper entity-type hack-map)))

(defn is?
  [this that]
  (= (select-pks (first this)) (select-pks (first that))))