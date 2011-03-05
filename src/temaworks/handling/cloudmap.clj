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

(defn children
  [cloud-map]
;;  {:pre [(not (nil? cloud-map))]}
  (cond
   (map? cloud-map)
   (val (first cloud-map))
   (is-type? cloud-map java.util.Map$Entry)
   (val cloud-map)
   :else nil))

(defn root
  [cloud-map]
  ;; {:pre [(not (nil? cloud-map))]}
  (cond
   (map? cloud-map)
   (key (first cloud-map))
   (is-type? cloud-map java.util.Map$Entry)
   (key cloud-map)
   :else
   nil))

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
      (fn [total value]
        (merge total
          (if (is-type? (key value) Attribute)
            (hash-map (:col-name (key value))
              (val value))
            (let [rel ((:rel (key value)))
                  mapping (clojure.set/map-invert (:fks-pks rel))
                  foreign (helper 
                            ((:to-entity rel)) 
                            (select-pks value))]
              (reduce merge
                {}
                (map (fn [entry]
                       (hash-map 
                         (get mapping (key entry))
                         (val entry)))
                  foreign))))))
      {}
      cloud-map))
  
  (helper (root  cloud-map) (children cloud-map)))

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
  (= (select-pks (children this)) (select-pks (children that))))