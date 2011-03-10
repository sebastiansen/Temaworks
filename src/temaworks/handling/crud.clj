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

;; Standardize on usage of cloud-crap within and amongst functions


(declare search-with-refs select-by-key select-by-fuzzy-example select-by-exact-example count-query cut-by-paging apply-sorting make-joins)

(def *cljql-identity* (=* 1 1))

(defn create
  [table-name record-map & recordmaps-maps]
  (let [persist-table  (table db/db table-name)]
    (try (conj! persist-table (map #(recordmap/record-map->hack-map %) (conj record-maps record-map)))
      (catch java.sql.SQLException e
        (.getErrorCode e)))))

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
            (where (apply and* (for [[k v] (recordmap/record-map->hack-map cloud-map)] (=* k v)))))
      (catch java.sql.SQLException e
        (.getErrorCode e)))))

(defn search-all
  [entity-type]
  (map #(recordmap/hack-map->record-map entity-type %) @(table db/db (:table-name entity-type))))

(defn exists?
  [record-map]
  (not (nil? @(->
                (recordmap/record-map->table record-map) 
                (select-by-exact-example
                  (:table-name (:entity-type record-map))
                  (recordmap/record-map->hack-map
                    (recordmap/record-map (:entity-type record-map)
                      (recordmap/select-pks record-map))))))))

(defn search-with-count
  [query entity-type]
  (vector (count-query query)
    (map #(recordmap/hack-map->record-map entity-type %) @query)))

(defn search-by-refs
  [query record-map]
  (-> (apply 
        select-by-fuzzy-example 
        (cons query (recordmap/record-map->hack-map record-map)))
    (make-joins record-map)
    (project [:*])))

(defn search-by-key
  [query record-map word]
  (select-by-key
    query
    (:table-name (:entity-type record-map))
    word
    (map #(:col-name %) 
      (filter #(= (:data-type %) String) (:atts (:entity-type record-map))))))


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
  ([example page per-page sort-field sort-order]
    (let [criteria-query (-> (cloudmap/cloud-map->table from)
                           ((search-with-criteria page per-page sort-field sort-order)))
          entity-type (cloudmap/tag from)]
      (if (empty? (cloudmap/children from))
        (search-with-count criteria-query entity-type)
        (search-with-count (search-with-refs criteria-query from to) entity-type))))
  
  ;; criteria + key
  ;; criteria + key + refs
  ([from to word page per-page sort-field sort-order]
    (let [key-query (-> (cloudmap/cloud-map->table from)
                      ((search-with-criteria page per-page sort-field sort-order))
                      (search-by-key from word))
          entity-type (cloudmap/root from)]
      (if (empty? (cloudmap/children from))
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

(defrecord Alias-tree
  [#^String prefix entity-type children])


(defn make-alias-tree 
  [prefix #^Record-map node]
  
  (Alias-tree. 
    prefix 
    (:entity-type node)
    (reduce merge
      {}
      (map
        (fn [entry]
          (hash-map 
            (recordmap/tag entry)
            (make-alias-tree
              (str prefix "_" (name (:key-name (recordmap/tag entry))))
              (val entry))))
        (filter recordmap/inner? (:children node)))
      
      )))


(defn am-has-children?
  [alias-map]
  (not-empty (:children alias-map))) 

(defn make-table
  "Constructs a table object from an aliases-tree node"
 [alias-map]	    	   
 (table (:table-name (:entity-type alias-map)) 
   (:prefix alias-map)))

(defn make-joins
  "Constructs ..."
  ([query #^recordmap.Record-map example]

     (defn- high-filter
       ([exprs val]
         (high-filter exprs val val))
       
       ([exprs val default]
         (cond
           (or (map? exprs) (not (coll? exprs)))
           (if (not= exprs val)
             exprs
             default)
           :else
           (let [filtered (filter #(not= % val) exprs)]
             (if (not-empty filtered)
               filtered
               default))
           )))
     
     
     (defn- make-alias
       "Constructs the aliased form keyword of a column corresponding to a table in the joins-tree"
       [node column]
       (let [prefix (if (string? node) ;; the base case
                      node
                      (:prefix node))] 
         (str (name prefix) "." (name column))))
     
     
     (defn- join-columns
       "Creates the equality predicates to join columns between two tables along a branch of the joins-tree"
       [source-node reference]

       (high-filter (reduce and*
                      (for [[local foreign] (:fks-pks ((:rel reference)))]
                        (=*
                          (make-alias source-node local)
                          (make-alias (get (:children source-node) reference) foreign))))
         (and*)
         *cljql-identity*))
     
     (defn- compare-value
       "Constructs a comparison predicate for a certain column and a value"
       [op alias-node attribute value]
       
       (op (make-alias destination-node
             (:col-name attribute))
         value))
     
     
     
     (defn- join-values
       "Constructs comparison predicates to restrict the concrete values of a certain join expression"
       ([destination-node example-node]
         (-> 
           (reduce
             and*
             *cljsql-identity*
             (high-filter
               (map
                 (fn [[tag value]]
                   (if (is-type? (recordmap/tag child) Interval)
                   (and*
                     (compare-value >=* destination-node (:att tag) (:from value))
                     (compare-value <=* destination-node (:att tag) (:to value)))
                   (compare-value =* destination-node tag value)))
                 (filter recordmap/leaf? (recordmap/children node)))
               *cljsql-identity*))
           
           (high-filter
             
             (and* *cljsql-identity* *cljsql-identity*)
             *cljsql-identity*)))             
           
     
     
     
     (defn- join-on
       "Generates the full join between two nodes of the tree"
       [query source-alias reference example-branch]
       (if-let [destination-alias (get (:children source-alias) 
                                    reference)]
         (join 
           query
           (make-table destination-alias)
           (where (and
                    (join-columns source-alias reference)
                    (join-values destination-alias example-branch))))
         query))
     
     
     
     (letfn [(join-children
               [query source-alias example-record]
               (reduce
                 (fn [acc-query child-node]
                   (join-branch
                     acc-query
                     source-alias
                     (recordmap/tag child-node)
                     (val child-node)))
                 query
                 (filter cloudmap/inner? (recordmap/children example-record))))
             
             (join-branch
               [query source-alias reference example-branch]    
               (join-children
                 (join-on query source-alias reference example-branch)
                 (get (:children destination-alias) reference)
                 example-branch))]
       
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
			  (and* (>=* (keyword (str (name table) "." (name k))) v) (<=* (keyword (str (name table) "." (name k))) (k to)))))))))))

;; hack-map -> query 
(defn select-by-exact-example
  [query table example]
  (select query 
	  (where
	   (reduce and* 
		   (for [[k v] example] (=* (keyword (str (name table) "." (name k))) v))))))
