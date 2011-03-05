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
    [temaworks.handling.cloudmap :as cloudmap]
    [temaworks.handling.db :as db])
  (:refer-clojure
   :exclude [compile distinct drop group-by take sort conj! disj!]))

;; Standardize on usage of cloud-crap within and amongst functions


(declare search-with-refs select-by-key select-by-fuzzy-example select-by-exact-example count-query cut-by-paging apply-sorting make-joins)

(def *cljql-identity* (=* 1 1))

(defn create
  [table-name cloud-map & cloud-maps]
  (let [persist-table  (table db/db table-name)]
    (try (conj! persist-table (map #(cloudmap/cloud-map->hack-map %) (conj cloud-maps cloud-map)))
      (catch java.sql.SQLException e
        (.getErrorCode e)))))

(defn update
  [table-name old-map new-map]
    (let [persist-table  (table db/db table-name)]
      (update-in!
        persist-table 
        (where (apply and* (for [[k v] (cloudmap/cloud-map->hack-map old-map)] (=* k v))))
        new-map)))

(defn delete
  [table-name cloud-map]
  (let [persist-table  (table db/db table-name)]
    (try @(disj! 
            persist-table 
            (where (apply and* (for [[k v] (cloudmap/cloud-map->hack-map cloud-map)] (=* k v)))))
      (catch java.sql.SQLException e
        (.getErrorCode e)))))

(defn search-all
  [entity-type]
  (map #(cloudmap/hack-map->cloud-map entity-type %) @(table db/db (:table-name entity-type))))

(defn exists?
  [cloud-map]
  (not (nil? @(->
                (cloudmap/cloud-map->table cloud-map) 
                (select-by-exact-example 
                  (:table-name (cloudmap/root cloud-map))
                  (cloudmap/cloud-map->hack-map {(cloudmap/root cloud-map) 
                                                 (cloudmap/select-pks (first cloud-map))}))))))

(defn search-with-count
  [query entity-type]
  (vector (count-query query)
    (map #(cloudmap/hack-map->cloud-map entity-type %) @query)))

(defn search-by-refs
  [query from to]
  (-> (apply 
        select-by-fuzzy-example 
        (cons query (map cloudmap/cloud-map->hack-map [from to])))
    (make-joins from to)
    (project [:*])))

(defn search-by-key
  [query cloud-map word]
  (let [entity-type (cloudmap/root cloud-map)]
    (select-by-key
      query
      (:table-name entity-type)
      word
      (map #(:col-name %) 
        (filter #(= (:data-type %) String) (:atts entity-type))))))


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
    (let [criteria-query (-> (cloudmap/cloud-map->table from)
                           ((search-with-criteria page per-page sort-field sort-order)))
          entity-type (cloudmap/root from)]
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

(defn at-root
  [node]  
  (cond
   (map? node)
   (key (first node))
   (is-type? node java.util.Map$Entry)
   (key node)
   :else
   node
   ))

(defn at-children
  [node]
  (cond
   (map? node)
   (vals node)
   (is-type? node java.util.Map$Entry)
   (val node)
   :else
   node))


(defn at-root?
  [node]
  (is-type? (:source (at-root node)) Entity-type))

(defn at-inner?
  [node]
  (and
   (not (is-type? (at-root node) Attribute))
   (map? (at-root node))
   (is-type? (:source (at-root node)) Reference)))

(defn make-alias-tree [prefix node]
  
  (defn- make-children [prefix name node]
    (hash-map
     {:prefix prefix
      :name name
      :source (cloudmap/root node)}
     (reduce merge 
	     (map
	      (fn [entry] (make-alias-tree
			   prefix
			   entry))
	      (cloudmap/children node)))))
    
  
  (cond
   ;; Provably wrong
   (cloudmap/leaf? node)
   {(cloudmap/root node) (cloudmap/children node)}
    
   (cloudmap/root? node)
   (make-children
    (name (:table-name (cloudmap/root node)))
    (:table-name (cloudmap/root node))
    node)
   
   :else
   ;; reference
    (let [reference (cloudmap/root node)]
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
                      (:prefix (at-root node)))] 
         (str (name prefix) "." (name column))))
    
     
     (defn- join-columns
       "Creates the equality predicates to join columns between two tables along a branch of the joins-tree"
       [source-node destination-node]
       (let [reference (:source (at-root destination-node))]
	 (high-filter (reduce and*
			      (for [[local foreign] (:fks-pks ((:rel reference)))]
				(=*
				 (make-alias source-node local)
				 (make-alias destination-node foreign))))
		      (and*)
		      *cljql-identity*)))
     
     (defn- compare-values
       "Constructs a parameterized comparison predicate for certain set of columns in a join expression"
       [op destination-node branch]
       (high-filter
	(reduce and*
		(map
		 (fn [node]
		   (op (make-alias destination-node
				   (:col-name (cloudmap/root node)))
		       (cloudmap/children node)))
		 (filter cloudmap/leaf?  (cloudmap/children branch))))
	(and*)
	*cljql-identity*))
     
     
     
     (defn- join-values
       "Constructs comparison predicates to restrict the concrete values of a certain join expression"
       ([destination-node from-branch]
         (let [] (compare-values =* destination-node from-branch)))
       ([destination-node from-branch to-branch]
         (println "por favor!!!")
         
         
         (-> 
	  (reduce and* 
		  (high-filter
		   (list (compare-values =*
					 destination-node
					 {(cloudmap/root from-branch)
					  (filter-keys (cloudmap/children from-branch)
						       (fn [node]  (not (contains?  (cloudmap/children  to-branch) node))))})
			 (compare-values >=*
					 destination-node
					 {(cloudmap/root from-branch)
					  (select-keys (cloudmap/children from-branch)
						       (keys (cloudmap/children to-branch)))})
			 (compare-values <=*
					 destination-node
					 {(cloudmap/root to-branch)
					  (cloudmap/children to-branch)}))
		   *cljql-identity*))
	  
	  (high-filter 
	   (and* *cljql-identity*)
	   *cljql-identity*))))
     
     
     
     (defn- join-on
       "Generates the full join between two nodes of the tree"
      [query source-alias destination-alias from-branch to-branch]
      (if (empty?  destination-alias)
        query
        (join query
          (make-table destination-alias)
          (where (and (join-columns source-alias destination-alias)
                   (join-values destination-alias from-branch to-branch))))))
     
     
     
     (letfn [(join-children
               [query source-alias from-branch to-branch]
               (let [alias-mapping  (dbg (reduce merge
                                           (map 
                                             (fn [node]				      
                                               {(:source  (at-root node)) node})
                                             (dbg (filter at-inner? (at-children source-alias))))))]
                 (reduce
		  (fn [acc-query child-node]
		    (join-branch acc-query
				 (dbg source-alias)
				 (alias-mapping (cloudmap/root child-node))
				 child-node
				 (find to-branch (dbg (cloudmap/root child-node)))))
		  
                   query
                   (dbg (filter cloudmap/inner? (cloudmap/children from-branch))))))
             (join-branch
	      [query source-alias destination-alias from-branch to-branch]    
	      (join-children
	       (join-on query source-alias destination-alias from-branch to-branch)
	       (dbg destination-alias)
	       from-branch
	       to-branch))]
       
       (join-children
	query
	(first (make-alias-tree "" from-tree))
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

(defn select-by-exact-example
  [query table example]
  (select query 
    (where
      (reduce and* 
        (for [[k v] example] (=* (keyword (str (name table) "." (name k))) v))))))
