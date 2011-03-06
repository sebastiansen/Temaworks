(ns temaworks.handling.crud
  (:use 
    clojureql.core
    clojureql.internal
    clojureql.predicates
    clojure.contrib.sql
    temaworks.meta.types
    temaworks.handling.aspect
    temaworks.handling.prueba)
  (:import [temaworks.meta.types Entity-type Attribute Reference Relationship])
  (:require clojure.set
    [temaworks.handling.recordmap :as recordmap]
    [temaworks.handling.db :as db])
  (:refer-clojure
   :exclude [compile distinct drop group-by take sort conj! disj!]))

;; Standardize on usage of record-crap within and amongst functions

(declare search-with-refs select-by-key select-by-fuzzy-example 
  select-by-exact-example count-query cut-by-paging apply-sorting make-joins)

(defn create
  [record-map & record-maps]
  (try (conj! (recordmap/record-map->table record-map) (map #(recordmap/record-map->hack-map %) (conj record-maps record-map)))
    (catch java.sql.SQLException e
      (.getErrorCode e))))

(defn update
  [table-name old-map new-map]
    (let [persist-table  (table db/db table-name)]
      (update-in!
        persist-table 
        (where (apply and* (for [[k v] (recordmap/record-map->hack-map old-map)] (=* k v))))
        new-map)))

(defn delete
  [table-name record-map]
  (let [persist-table  (table db/db table-name)]
    (try @(disj! 
            persist-table 
            (where (apply and* (for [[k v] (recordmap/record-map->hack-map record-map)] (=* k v)))))
      (catch java.sql.SQLException e
        (.getErrorCode e)))))

(defn search-all
  [entity-type]
  (map #(recordmap/hack-map->record-map entity-type %)
    @(table db/db (:table-name entity-type))))

(defn exists?
  [record-map]
  (not (nil? @(->
                (recordmap/record-map->table record-map) 
                (select-by-exact-example 
                  (:table-name (recordmap/root record-map))
                  (recordmap/record-map->hack-map {(recordmap/root record-map) 
                                                 (recordmap/select-pks (first record-map))}))))))

(defn search-with-count
  [query entity-type]
  (vector (count-query query)
    (map #(recordmap/hack-map->record-map entity-type %) @query)))

(defn search-by-refs
  [query from to]
  (-> (apply 
        select-by-fuzzy-example 
        (cons query (map recordmap/record-map->hack-map [from to])))
    (make-joins from to)
    (project [:*])))

(defn search-by-key
  [query record-map word]
  (let [entity-type (recordmap/root record-map)]
    (if-let [atts (dorun (filter #(= (:data-type %) String) (:atts entity-type)))]
      (select-by-key
        query
        (:table-name entity-type)
        word
        (map #(:col-name %) atts))
      query)))


(defn search-with-criteria
  
  ([page per-page sort-field sort-order]
    (fn [query]
      (-> (cut-by-paging query page per-page) 
        (#(if sort-field
            (apply-sorting % sort-field sort-order)
            %)))))
  
  ([from page per-page sort-field sort-order]
    (search-with-criteria from nil page per-page sort-field sort-order))
  
  ;; criteria
  ;; criteria + refs
  ([from to page per-page sort-field sort-order]
    (let [criteria-query (-> (recordmap/record-map->table from)
                           ((search-with-criteria page per-page sort-field sort-order)))
          entity-type (recordmap/root from)]
      
      (if (empty? (recordmap/children from))
        (search-with-count criteria-query entity-type)
        (search-with-count (search-with-refs criteria-query from to) entity-type))))
  
  ;; criteria + key
  ;; criteria + key + refs
  ([from to word page per-page sort-field sort-order]
    (let [key-query (-> (recordmap/record-map->table from)
                      ((search-with-criteria page per-page sort-field sort-order))
                      (search-by-key from word))
          entity-type (recordmap/root from)]
      (if (empty? (recordmap/children from))
        (search-with-count key-query entity-type)
        (search-with-count (search-with-refs key-query from to) entity-type)))))

(defn count-query
  [query]
  (second (ffirst @(aggregate query [:count/*]))))

(defn cut-by-paging
  [query page per-page]
  (-> (drop query (int (* page per-page)))
    (limit per-page)))

(defn apply-sorting 
  [query sort-field sort-order] 
  (sort query [(keyword (str (name sort-field) sort-order))]))

(defn at-root
  [node]
  (println node)
  (if (map? node)
    (key (first node))
    (key node)))

(defn at-children
  [node]
  (if (map? node)
    (vals node)
    (val node)))


(defn at-root?
  [node]
  (is-type? (:source (at-root node)) Entity-type))

(defn at-inner?
  [node]
  (and
    (not (is-type? (at-root node) Attribute))
    (is-type? (:source (at-root node)) Reference)))

(defn make-alias-tree [prefix node]
  
  (defn- make-children [prefix name node]
    (hash-map
      {:prefix prefix
       :name name
       :source (recordmap/root node)}
      (reduce merge 
        (map
          (fn [entry] (make-alias-tree
                        prefix
                        entry))
          (recordmap/children node)))))
    
  (cond
   ;; Provably wrong
   (recordmap/leaf? node)
   {(recordmap/root node) (recordmap/children node)}
    
   (recordmap/root? node)
   (make-children
    (:table-name (recordmap/root node))
    (:table-name (recordmap/root node))
    node)
   
   :else
   ;; reference
    (let [reference (recordmap/root node)]
      (make-children 
       (str prefix "_" (name (:key-name reference)))
       (table-name reference)
       node))))

(defn make-table
  "Constructs a table object from an aliases-tree node"
 [node]	    	   
 (table {(:name (at-root node)) (:prefix (at-root node))}))

(defn make-joins
  "Constructs ..."
  ([query [from-tree to-tree]]
    (defn- make-alias
      "Constructs the aliased form keyword of a column corresponding to a table in the joins-tree"
      [node column]
      (let [prefix (if (string? node) ;; the base case
                     node
                     (:prefix node))] 
        (keyword (str prefix "." (name column)))))
    
    
    (defn- join-columns
      "Creates the equality predicates to join columns between two tables along a branch of the joins-tree"
      [source-node destination-node]
      (let [reference (:source destination-node)]
        (reduce and*
          (for [[local foreign] (:fks-pks ((:rel reference)))]
                (=*
                  (make-alias source-node local)
                  (make-alias destination-node foreign))))))
      
    (defn- compare-values
      "Constructs a parameterized comparison predicate for certain set of columns in a join expression"
      [op destination-node branch]
      (reduce and*
        (map
          (fn [node]
            (op (make-alias destination-node
                  (:col-name (recordmap/root node)))
              (recordmap/children node)))
          (filter recordmap/leaf? (recordmap/children branch)))))
    
    
    (defn- join-values
      "Constructs comparison predicates to restrict the concrete values of a certain join expression"
      ([destination-node from-branch]
        (compare-values =* destination-node from-branch))
      ([destination-node from-branch to-branch]	  
        (reduce and* (list (compare-values =*
                             destination-node
                             (filter-keys from-branch
                               #(not (contains? to-branch %))))
                       (compare-values >=*
                         destination-node
                         (select-keys from-branch
                           (keys to-branch)))
                       (compare-values <=*
                         destination-node
                         to-branch)))))
    
    (defn- join-on
      "Generates the full join between two nodes of the tree"
      [query source-alias destination-alias from-branch to-branch]
      (join query
        (make-table destination-alias)
        (where (and (join-columns source-alias
                      destination-alias)
                 (join-values destination-alias from-branch to-branch)))))
    
    
    (letfn [(join-children
              [query source-alias from-branch to-branch]
              (let [alias-mapping (reduce merge
                                    (map 
                                      (fn [node]				      
                                        {(:source (at-root node)) node})
                                      source-alias))]
                (first (reduce (fn [[acc-query source-alias] child-node]
                                 (vector (join-branch acc-query
                                           source-alias
                                           (alias-mapping child-node)
                                           child-node
                                           (find to-branch (recordmap/root child-node)))
                                   (alias-mapping child-node)))
                         [query source-alias]
                         (recordmap/children from-branch)))))
            (join-branch
              [query source-alias destination-alias from-branch to-branch]
              (join-children
                (join-on query source-alias destination-alias from-branch to-branch)
                source-alias
                from-branch
                to-branch))]
      
       (join-children
         query
         (make-alias-tree "" from-tree)
         from-tree
         to-tree))))

(defn select-by-key
  [query table word atts]  
  (select query 
    (where 
      (apply or* 
        (map (fn [x] (like (keyword (str (name table) "." (name x))) word)) atts)))))

(defn select-by-fuzzy-example
  ([query table from]
    (select-by-fuzzy-example query table from nil))
  ([query table from to]
    (select query 
      (where 
        (apply and* 
          (for [[k v] from]
            (cond 
              (string? v)
              (like (keyword (str (name table) "." (name k))) v)
              
              (map? v)
              (apply and*
                (if (nil? (k to))
                  (for [[func value] v]
                    (let [app-func (keyword (str func "/" (name (keyword (str (name table) "." (name k))))))]
                      (=* app-func value)))
                  (for [[func value] v]
                    (let [app-func (keyword (str func "/" (name (keyword (str (name table) "." (name k))))))
                          to-func ((k to) func)]
                      (if (nil? to-func)
                        (=* app-func value)
                        (and* (>=* app-func value) (<=* app-func to-func)))))))
              
              :else
              (if (nil? (k to))
                (=* (keyword (str (name table) "." (name k))) v)
                (and* (>=* (keyword (str (name table) "." (name k))) v) 
                  (<=* (keyword (str (name table) "." (name k))) (k to)))))))))))

(defn select-by-exact-example
  [query table example]
  (select query 
    (where
      (reduce and* 
        (for [[k v] example] (=* (keyword (str (name table) "." (name k))) v))))))