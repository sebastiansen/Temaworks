(ns meta.types
  (:import (org.zkoss.zul
            Textbox Intbox
            Checkbox Combobox
            Button Image)))

(declare pks)

(defrecord Entity-type
  [table-name                 ;Name of table (keyword)
   single-name                ;Single name for  
   multi-name                 ;Single name for  
   atts                       ;Attributes
   refs                       ;References corresponing to relationships
   in-menu                    ;
   form-order                 ;the name of reference-bound key can't be the same as that of an attribute
   selector-order             ;ditto
   search-order               ;same as selector-order
   filter-ref                 ;
   to-str                     ;
   operations                 ;global operations :search? :create? :update? :delete?
   auto-inc-pk?
   icon])               

(defrecord Reference
  [ref-name
   rel
   direction
   in-form-type
   aggregates
   ;computable?
   ])

(defrecord Attribute
  [att-name 
   data-type 
   widget-type 
   pk? 
   not-null?
   computable?
   aggregates])

(defrecord Relationship
  [from-entity 
   to-entity 
   pk?
   not-null?
   card
   fks-pks
   mapping-entity])

(defn att-pks
  [entity-type]
  (map #(key %) (filter #(:pk? (val %)) (:atts entity-type))))

(defn ref-pks
  [entity-type]
  (map #(key %) (filter #(let [rel ((:rel (val %)))]
                           (and 
                             (= (:direction (val %)) :from)
                             (not= (:card rel) :many-to-many)
                             (:pk? rel))) (:refs entity-type))))

(defn to-pks-ref 
  [fks-pks r]
  (apply 
    merge 
    (for [[fk pk] fks-pks] 
      (hash-map pk (fk r)))))

(defn to-fks-ref 
  [fks-pks r]
  (apply 
    merge 
    (for [[fk pk] fks-pks] 
      (hash-map fk (pk r)))))

(defn is?
  [entity-type this that]
  (let [pkz (pks entity-type)]
    (= (select-keys this pkz) (select-keys that pkz))))

(defn pks
  [entity-type]
  (let [refs (ref-pks entity-type)]
    (if (empty? refs) 
      (att-pks entity-type)
      (apply conj (att-pks entity-type) 
        (apply conj [] (map #(keys(:fks-pks((:rel (% (:refs entity-type)))))) 
                         refs))))))

(defn pks-att-ref
  [entity-type] 
  (let [r (ref-pks entity-type)]
    (if  (empty? r)
      (att-pks entity-type) 
      (apply conj (att-pks entity-type) r))))
   
(def widget-types
  {:textbox "org.zkoss.zul.Textbox"
   :intbox "org.zkoss.zul.Intbox"
   :longbox "org.zkoss.zul.Longbox"
   :textarea "org.zkoss.zul.Textbox"
   :checkbox "org.zkoss.zul.Checkbox"
   :datebox "org.zkoss.zul.Datebox"
   :timebox "org.zkoss.zul.Timebox"
   :radiogroup "org.zkoss.zul.Radiogroup"
   :combobox "org.zkoss.zul.Combobox"})

(def input-formats
  {:rut "#.###.###-#"
   :number "#,###,###"
   :decimal "#.##0,##"
   })

