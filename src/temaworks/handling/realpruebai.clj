(ns temaworks.handling.realpruebai
  (:use
    temaworks.meta.types
    clojureql.core
    clojureql.internal
    clojureql.predicates
    temaworks.handling.crud
    temaworks.handling.cloudmap)
  (:require clojure.zip)
  (:import [temaworks.meta.types Entity-type Attribute Reference Relationship])
  (:require [temaworks.handling.db :as db]) 
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
       "Date" :date  java.util.Date :textbox true true false nil)
     ]
    
    [(Reference.
       "Origen"
       :origin
       #(origin-city)
       :from
       :combobox
       nil)
     (Reference.
       "Destino"
       :destination
       #(destination-city)
       :from
       :combobox
       nil
       )]   
    true
    [(Reference.
       "Origen"
       :origin
       #(origin-city)
       :from
       :combobox
       nil)
     (Reference.
       "Destino"
       :destination
       #(destination-city)
       :from
       :combobox
       nil
       )] 
    [(Attribute.
       "Date" :date  java.util.Date :textbox true true false nil)]
    []
    nil
    #("")
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
   []
   []
   (Reference.
      "State"
      :state
      #(city-has-state)
      :from
      :combobox
      nil)
   #(get (val (first %)) (Attribute.
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


(let [cloudcrap
      (hack-map->cloud-map (travel)  {
                                      :origin_city "Los Angeles"
                                      :destination_state "California"})]
  
  (-> (table db/db :travel)
    (make-joins [cloudcrap]))
 )
