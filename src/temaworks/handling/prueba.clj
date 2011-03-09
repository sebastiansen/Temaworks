(ns temaworks.handling.prueba
  (:use
    temaworks.meta.types
    clojureql.core
    clojureql.internal
    clojureql.predicates
    temaworks.handling.db)
  (:require clojure.zip)
  (:import [temaworks.meta.types Entity-type Attribute Reference Relationship Interval Tag Operation App])
  
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

(let [origin      (Reference.
                      "Origen"
                      :origin
                      #(origin-city)
                      :from
                      :ref-box
                      nil)
      destination (Reference.
                    "Destino"
                    :destination
                    #(destination-city)
                    :from
                    :combobox
                    nil)
      create-op   (Tag. "Crear viaje" 
                    [(Operation.
                       "Cosasasasa"
                       #(travel)
                       :create
                       :wewa
                       "asfasf")
                     (Operation.
                       "Creación masiva"
                       #(travel)
                       :bulk-create
                       :wewa
                       "asfasf")]
                    :travel 
                    "Vender pasajes")]

  (defn travel []
    (Entity-type. 
      :travel "Travel" "Travels"
      
      [(Attribute.
         "Date" :date  java.util.Date :datebox true true false nil)]
      
      [origin
       destination]   
      true
      (with-meta 
        [origin
         destination
         (Attribute.
           "Date" :date  java.util.Date :datebox true true false nil)
         (Attribute.
           "File" :file java.util.Date :filebox true true false nil)] {:specs [:constrain]}) 
      [(Attribute.
         "Date" :date  java.util.Date :datebox true true false nil)]
      (with-meta 
        [origin
         destination
         (Interval.
           (Attribute.
             "Date" :date  java.util.Date :datebox true true false nil))] {:specs [:optional]})
      (with-meta 
        [origin
         destination
         (Attribute.
           "Date" :date  java.util.Date :datebox true true false nil)] {:specs [:optional]})
      nil
      #(get (:values %) 
         (Attribute.
           "Date" :date  java.util.Date :datebox true true false nil))
      [(Operation. "Buscar" #(travel) :search :aea "Buscar WEAS") create-op]
      false
      :group
      "Travel")))

(let [stato (Reference.
              "State"
              :state
              #(city-has-state)
              :from
              :combobox
                nil)]
  (defn city
    []
    (Entity-type. 
      :city "City" "Cities"
      
      [(Attribute.
         "Name" :name String :textbox true true false nil)]
      
      [stato]   
      false
      [] 
      [(Attribute.
      "Name" :name String :textbox true true false nil)]
      []
      []
      stato
      #(get (:values %) 
         (Attribute.
           "Name" :name String :textbox true true false nil))
      nil
      false
      :group
      nil)))

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
    []
    nil
    #(get (:values %) (Attribute.
                        "Name" :name String :textbox true true false nil))
    nil
    false
     :group
     nil))

(def app 
  (App.
    "Wladibus"
    [(travel) (Tag. "Cosa" [(Tag. "poto" nil :caca "POTITO")] :wdwa "COSITAAA")]
    nil
    :tree
    :wea
    "POTOBUS"
    :north))