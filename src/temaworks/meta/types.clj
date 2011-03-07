(ns temaworks.meta.types
  (:import (org.zkoss.zul
            Textbox Intbox
            Checkbox Combobox
            Button Image)))

(defrecord Entity-type
  [table-name                 ;; Name of table (keyword)
   single-name                ;; Single name  
   multi-name                 ;; Plural name for  
   atts                       ;; Attributes
   refs                       ;; References corresponing to relationships
   in-menu                    ;; Specifies if this entity appear in the global menu
   form-order                 ;; (vector) 
   selector-order             ;; (vector) 
   search-order               ;; (vector)
   filter-ref                 ;;   
   to-str                     ;; 
   operations                 ;; Global operations :search? :create? :update? :delete?
   auto-inc-pk?               ;;
   ;; on-create
   ;; on-update
   icon])

(defrecord Relationship
  [from-entity 
   to-entity 
   pk?
   not-null?
   card
   fks-pks
   mapping-rel])

(defrecord Mapping-rel
  [from-ref
   to-ref])

(defrecord Reference
  [ref-name
   key-name
   rel
   widget-type
   mutual-ref
   computable?])

(defrecord Many-ref
  [ref-name
   key-name
   rel
   widget-type
   mutual-ref
   computable?])

(defrecord Attribute
  [att-name
   col-name
   data-type
   widget-type
   pk?
   not-null?
   computable?
   aggregates])

(defrecord Interval
  [att])

(derive Reference ::ref-type)
(derive Many-ref ::ref-type)

(def widget-types
  {:textbox "org.zkoss.zul.Textbox"
   :intbox "org.zkoss.zul.Intbox"
   :longbox "org.zkoss.zul.Longbox"
   :textarea "org.zkoss.zul.Textbox"
   :checkbox "org.zkoss.zul.Checkbox"
   :datebox "org.zkoss.zul.Datebox"
   :timebox "org.zkoss.zul.Timebox"
   :radiogroup "org.zkoss.zul.Radiogroup"
   :combobox "org.zkoss.zul.Combobox"
   :spinner "org.zkoss.zul.Spinner"})

(def input-formats
  {:rut "#.###.###-#"
   :number "#,###,###"
   :decimal "#.##0,##"})

(defn is-type? [value datatype] (isa? (type value) datatype))

(defn table-name
  [instance]
  (cond 
    (is-type? instance Entity-type)
    (:table-name instance)
    (is-type? instance Reference)
    (:table-name ((:to-entity ((:rel instance)))))
    :else
    nil))