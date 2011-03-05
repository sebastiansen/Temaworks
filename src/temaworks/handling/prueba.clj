(ns temaworks.handling.prueba
  (:use
    temaworks.meta.types
    clojureql.core
    clojureql.internal
    clojureql.predicates
    temaworks.handling.db)
  (:require clojure.zip)
  (:import [temaworks.meta.types Entity-type Attribute Reference Relationship])
  
  (:refer-clojure
    :exclude [compile distinct drop group-by take sort conj! disj! < <= > >= =]))

(declare travel city state)


(defn origin-city
  []
  (Relationship.
   #(travel) #(city) true true :many-to-one {:origin_city :name :origin_state :state} nil))

(defn destination-city
  []
  (Relationship.
   #(travel) #(city) true true :many-to-one {:destination_city :name :destination_state :state} nil))


(defn city-has-state
  []
  (Relationship.
    #(city) #(state) true true :many-to-one {:state :name} nil))

(def travel
    (fn [] (Entity-type. 
    :travel "Travel" "Travels"
    
    [(Attribute.
       "Date" :date  java.util.Date :datebox true true false nil)
     ]
    
    [(Reference.
       "Origen"
       :origin
       #(origin-city)
       :from
       :ref-box
       nil)
     (Reference.
       "Destino"
       :destination
       #(destination-city)
       :from
       :combobox
       nil)]   
    true
    [(Reference.
       "Origen"
       :origin
       #(origin-city)
       :from
       :ref-box
       nil)
     (Reference.
       "Destino"
       :destination
       #(destination-city)
       :from
       :combobox
       nil)
     (Attribute.
       "Date" :date  java.util.Date :datebox true true false nil)] 
    [(Attribute.
       "Date" :date  java.util.Date :datebox true true false nil)]
    (array-map (Reference.
       "Origen"
       :origin
       #(origin-city)
       :from
       :ref-box
       nil) false
     (Reference.
       "Destino"
       :destination
       #(destination-city)
       :from
       :combobox
       nil
       ) false
     (Attribute.
       "Date" :date  java.util.Date :datebox true true false nil) true)
    nil
    #(get (val (first %)) 
      (Attribute.
       "Date" :date  java.util.Date :datebox true true false nil))
    nil
    false
    :group)))

(defn city
  []
  (Entity-type. 
   :city "City" "Cities"
    
   [(Attribute.
      "Name" :name String :textbox true true false nil)
    ]
   
   [(Reference.
      "State"
      :state
      #(city-has-state)
      :from
      :combobox
      nil)]   
   false
   [] 
   [(Attribute.
      "Name" :name String :textbox true true false nil)]
   []
   (Reference.
      "State"
      :state
      #(city-has-state)
      :from
      :combobox
      nil)
   #(get (val (first %)) 
      (Attribute.
        "Name" :name String :textbox true true false nil))
   nil
   false
   :group))

(defn state
  []
  (Entity-type. 
    :state "State" "States"
    
     [(Attribute.
        "Name" :name String :textbox true true false nil)
      ]
     {}   
     false
     [] 
     []
     []
     nil
     #(get (val (first %)) (Attribute.
                             "Name" :name String :textbox true true false nil))
     nil
     false
     :group))

(def types [(state) (city) (travel)])

(def initial-view {:view-type :selector :entity-type (travel)})

(def app-name "Wladibus")

(comment (let [cloudcrap
               (hack-map->cloud-map (travel)  {:date (java.util.Date.) 
                                               :origin_city "Seattle"
                                               :origin_state "Washington"})]
           
           (-> (table db :buses)
             (make-joins cloudcrap))))


; #:temaworks.meta.types.Entity-type{:table-name :travel, :single-name Travel, :multi-name Travels, :atts [#:temaworks.meta.types.Attribute{:att-name Date, :col-name :date, :data-type java.util.Date, :widget-type :textbox, :pk? true, :not-null? true, :computable? false, :aggregates nil}], :refs [#:temaworks.meta.types.Reference{:ref-name Origen, :key-name :origin, :rel #<prueba$travel$fn__4499 temaworks.handling.prueba$travel$fn__4499@1ab2ddb>, :direction :from, :widget-type :combobox, :mutual-ref nil} #:temaworks.meta.types.Reference{:ref-name Destino, :key-name :destination, :rel #<prueba$travel$fn__4501 temaworks.handling.prueba$travel$fn__4501@14fb3e1>, :direction :from, :widget-type :combobox, :mutual-ref nil}], :in-menu true, :form-order [#:temaworks.meta.types.Reference{:ref-name Origen, :key-name :origin, :rel #<prueba$travel$fn__4503 temaworks.handling.prueba$travel$fn__4503@1aa9b4d>, :direction :from, :widget-type :combobox, :mutual-ref nil} #:temaworks.meta.types.Reference{:ref-name Destino, :key-name :destination, :rel #<prueba$travel$fn__4505 temaworks.handling.prueba$travel$fn__4505@1b9ef8c>, :direction :from, :widget-type :combobox, :mutual-ref nil}], :selector-order [#:temaworks.meta.types.Attribute{:att-name Date, :col-name :date, :data-type java.util.Date, :widget-type :textbox, :pk? true, :not-null? true, :computable? false, :aggregates nil}], :search-order [], :filter-ref nil, :to-str #<prueba$travel$fn__4507 temaworks.handling.prueba$travel$fn__4507@1e037a>, :operations nil, :auto-inc-pk? false, :icon :group}
; false


;(defn- make-alias-tree [prefix entry entity-type reference])


; non-memoized "Elapsed time: 2975.442949 msecs"
; memoized "Elapsed time: 2294.975854 msecs"

; 50000000 
; non "Elapsed time: 12526.877935 msecs"
; mem "Elapsed time: 2137.530844 msecs"
;(time (dotimes [n 50000000]
 ;       (travel)))


;{:date #<Date 2011-02-21>, 
;:state California, 
;:name San Francisco, 
;:origin_city San Francisco, 
;:origin_state California, 
;:destination_state Washington, 
;:destination_city Seattle}
;{#:temaworks.meta.types.Reference{:ref-name Origen, :rel #<prueba$fn__3178$fn__3179 temaworks.handling.prueba$fn__3178$fn__3179@133b16a>, :direction :from, :widget-type :combobox, :aggregates nil} {#:temaworks.meta.types.Attribute{:att-name Name, :col-name :name, :data-type java.lang.String, :widget-type :textbox, :pk? true, :not-null? true, :computable? false, :aggregates nil} San Francisco}}

;{#:temaworks.meta.types.Reference{:ref-name Destino, :rel #<prueba$fn__3434$fn__3437 temaworks.handling.prueba$fn__3434$fn__3437@13cf887>, :direction :from, :widget-type :combobox, :aggregates nil} {#:temaworks.meta.types.Reference{:ref-name State, :rel #<prueba$city$fn__3442 temaworks.handling.prueba$city$fn__3442@199f755>, :direction :from, :widget-type :combobox, :aggregates nil} {#:temaworks.meta.types.Attribute{:att-name Name, :col-name :name, :data-type java.lang.String, :widget-type :textbox, :pk? true, :not-null? true, :computable? false, :aggregates nil} Washington}, #:temaworks.meta.types.Attribute{:att-name Name, :col-name :name, :data-type java.lang.String, :widget-type :textbox, :pk? true, :not-null? true, :computable? false, :aggregates nil} Seattle}}

;#:temaworks.meta.types.Reference{:ref-name Origen, :rel #<prueba$fn__3690$fn__3691 temaworks.handling.prueba$fn__3690$fn__3691@1c935cc>, :direction :from, :widget-type :combobox, :aggregates nil}


; #:temaworks.meta.types.Entity-type{:table-name :travel, :single-name Travel, :multi-name Travels, :atts [#:temaworks.meta.types.Attribute{:att-name Date, :col-name :date, :data-type java.util.Date, :widget-type :textbox, :pk? true, :not-null? true, :computable? false, :aggregates nil}], :refs [#:temaworks.meta.types.Reference{:ref-name Origen, :key-name :origin, :rel #<prueba$travel$fn__4351 temaworks.handling.prueba$travel$fn__4351@ed41f8>, :direction :from, :widget-type :combobox, :mutual-ref nil} #:temaworks.meta.types.Reference{:ref-name Destino, :key-name :destination, :rel #<prueba$travel$fn__4353 temaworks.handling.prueba$travel$fn__4353@863cc1>, :direction :from, :widget-type :combobox, :mutual-ref nil}], :in-menu true, :form-order [#:temaworks.meta.types.Reference{:ref-name Origen, :key-name :origin, :rel #<prueba$travel$fn__4355 temaworks.handling.prueba$travel$fn__4355@fea539>, :direction :from, :widget-type :combobox, :mutual-ref nil} #:temaworks.meta.types.Reference{:ref-name Destino, :key-name :destination, :rel #<prueba$travel$fn__4357 temaworks.handling.prueba$travel$fn__4357@1145cc>, :direction :from, :widget-type :combobox, :mutual-ref nil}], :selector-order [#:temaworks.meta.types.Attribute{:att-name Date, :col-name :date, :data-type java.util.Date, :widget-type :textbox, :pk? true, :not-null? true, :computable? false, :aggregates nil}], :search-order [], :filter-ref nil, :to-str #<prueba$travel$fn__4359 temaworks.handling.prueba$travel$fn__4359@1fd5730>, :operations nil, :auto-inc-pk? false, :icon :group}


;; java.lang.RuntimeException: java.lang.IllegalArgumentException: No matching clause: class temaworks.meta.types.Reference (prueba.clj:55)