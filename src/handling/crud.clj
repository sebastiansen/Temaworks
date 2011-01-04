(ns handling.crud
  (:use clojureql.core
	clojureql.internal
	clojureql.predicates
	clojure.contrib.sql)
  (:refer-clojure
   :exclude [compile distinct drop group-by take sort conj! disj! < <= > >= =]))

(def db
     {:classname   "com.mysql.jdbc.Driver"
      :subprotocol "mysql"
      :user        "root"
      :password    "grtrps"
      :auto-commit true                                    
      :subname     "//localhost:3306/generales"})

(defn search
  
  ([table-name]
     (let [persist-table  (table db table-name)]
       @(-> persist-table)))
  
  ([table-name record]
     (let [persist-table  (table db table-name)]
       @(-> persist-table 
	    (select (where (apply and (for [[k v] record] (= k v))))))))

  ([table-name page per-page sort-field sort-order]
     (let [persist-table  (table db table-name)]
       (vector
        (second (ffirst @(-> persist-table (aggregate [:count/*]))))
	@(-> persist-table
	     (drop (* page per-page))
	     (limit per-page)
	     (#(if (nil? sort-field) % (sort % [(keyword (str (name sort-field) sort-order))])))))))
  
  ([condition table-name page per-page sort-field sort-order]
     (let [persist-table  (table db table-name)]
       (vector 
        (second (ffirst @(-> persist-table (aggregate [:count/*]) (select (condition)))))
	@(-> persist-table 
             (select (condition))
             (drop (int (* page per-page)))
             (limit per-page)
             (#(if (nil? sort-field) % (sort % [(keyword (str (name sort-field) sort-order))]))))))))

(defn exists?
  [table-name ids]
  (not (nil? (search table-name ids))))

(defn create
  [table-name record]
  (let [persist-table  (table db table-name)]
    @(conj! persist-table record)))

(defn update
  [table-name old-record new-record]
  (let [persist-table  (table db table-name)]
    @(update-in! persist-table (where (apply and (map #(= (key %) (val %)) old-record))) new-record)))

(defn delete
  [ids table-name]
  (let [persist-table  (table db table-name)]
    @(disj! persist-table 
	    (where (apply and (map #(= (first %) (second %)) ids))))))

(defn search-by-key-example
  [word atts example & criteria]
  (apply search (apply conj criteria [#(where (and (apply or (map (fn [x](like x word)) atts)) 
						   (apply and (for [[k v] example] (if (string? v) (like k v) (= k v))))))])))
(defn search-by-key
  [word atts & criteria]
  (apply search (apply conj criteria [#(where (apply or (map (fn [x](like x word)) atts)))])))

(defn search-by-example
  [example & criteria]
  (apply search (apply conj criteria [#(where (apply and (for [[k v] example] (if (string? v) (like k v) (= k v)))))])))