(ns temaworks.meta.subject)

;;defentity

;;defattribute

;;defreference

;;defrelationship

;; We show here that the stack monad obeys the subject laws,
;; as per the Wladimir-JP isomorphism, which can be expressed
;; succintly with an if.

;;make-constraint

;; Bindings... motherfucker! How will we do them?
;; * Monadic crap
;; * Let 'n' gensym (ewwww!)
;; * Lotsa lotsa refs

(let-subject buses-wlad

  (entity city
    :auto-inc-pk 0 inc "ID" ;; o podría ser por defecto JPesco
    (atts 
      nombre [:display "Nombre"
              :type String])
    (entity state ;; genera una relationship city-has-state y una entity city.state o state?
      :display "State"
      :constraints :pk
      :cardi :m-to-o ;; o-to-m por defecto?
      :auto-inc-pk 0 inc "ID"
      (atts
        nombre [:display "Nombre"
                :type String])))
  
  (entity travel
    :auto-inc-pk 0 #(+ % 5) "ID"
    :display "Viaje"
    
    (rel origen ;; genera una relationship travel-origen-city ? 
      :display "Origen"
      :cardi :m-to-o
      :constraints :not-nil
      :widget :combobox)
    
    (rel destino ;; genera una relationship travel-destination-city ? 
      :display "Destino"
      :cardi :m-to-o
      :constraints :not-nil
      :widget :combobox)
    
    (atts 
      fecha-salida [:display "Fecha Salida"
                    :type Timestamp
                    :constraints :not-nil] ;; widget :datetime por defecto
      
      fecha-llegada [:display "Fecha Llegada"
                    :type Timestamp
                    :constraints :not-nil
                    :computable #()]
      
      asientos-disponibles [:display "Asientos disponibles"
                            :display-only
                            :function #(count 
                                         (filter #(nose-que-wea % disponible?) seats)) ;; identificar seats como collection
                            ;; :type String por defecto
                            ;; y se aplica to-str al resultado de la función 
                            ])
    (entity seats
      :display "Seats"
      :cardi :o-to-m
      :bidir)
    
    :form-order [:id origen destino fecha-salida fecha-llegada seats] ;; :id si tiene un auto-inc
    :search-order [origen destino (with-algo fecha-salida 
                                    :interval
                                    :widget :datewea)]
    
    (constraint ;; opcion 1
      (and (not= origin destino) (not= fecha-salida fecha-llegada)))
    
    (constraint ;; opcion 2
      (not= origin destino) ;; activar por eventos de widgets?
      "Ciudad de origen no puede ser igual a la de destino")
    
    (on-create
      (if (= (access travel city name) "San Francisco")
        ()
        ()))))

(defmacro defrel
  [bind-name from-entity to-entity specs]
  )

(defn gen-scheme)

(defmacro let-subject
  [& defs])

(defmacro defentity
  [])