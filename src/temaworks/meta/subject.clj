(ns temaworks.meta.subject)

{}

(def default-widgets
  {String        :textbox
   Integer       :intbox
   Double        :doublebox
   Long          :longbox
   Boolean       :checkbox
   Date          :datebox
   Timestamp     :datetime
   Media         :filebox
   :function     :textbox})

(def operations-types
  {:update
  :form-update
  :task
  :selector
  :show
  :create
  :tag 876})

(defmacro entity
  [single-name specs]
  (let [{:auto-inc-pk (fn []
                        )
         }]
    (new-rec Entity-type
      )))

(let-subject buses-wlad
  
  (state                                  
    {:display ["State" "States"]
     :constraints :pk                      
     :auto-inc-pk [0 inc "ID"]
     :atts {nombre {:display "Nombre"
                    :type String}}})

  (entity city
    {:auto-inc-pk [0 inc "ID"]                     ;; o podría ser por defecto JPesco
     :atts {nombre 
            {:display "Nombre"
             :type String}}})
  
  (entity ticket
    {:display ["Venta Pasaje" " Venta Pasajes"]
     
     :atts
     {disponible? 
      {:display "Disponible"
       :type Boolean}}
     
     :refs
     {seat
      {:display "Asiento"
       :entity  :seat
       :widget  {:type :ref-form
                :aggs {:order [:numero]}}}
      travel
      {:display "Viaje"
       :entity  :travel
       :widget  {:type :ref-form
                 :aggs {:order [:origen :destino :fecha-salida :fecha-llegada]}}}}
     
     :operations
     {"Realizar Venta"
      {:type        :update
       :action-name "Finalizar Venta"
       :order       [seat travel pay-type]
       :before      [#(=> % disponible? false)]}
      
      "Cancelar Pasaje"
      {:type        :update 
       :action-name "Cancelar Pasaje"
       :order       [seat travel]
       :before      [#(=> % disponible? true)]}}})
  
  (entity seat
    {:display ["Asiento" "Asientos"]
     :atts
     {numero {:display "N°"
              :type Integer
              :constraints :not-nil}}
     :refs
     {bus {:display "Bus"}}})
  
  (entity bus
    {:fns
     {disponible? 
      (fn [b begin end]
        (empty?
          @(make-joins 
             (assoc-val b travel 
               (record-map travel 
                 {:fecha-salida  {:from begin :to end}
                  :fecha-llegada {:from begin :to end}})))))}})
  
  (entity travel
    {:auto-inc-pk [0 inc "ID"]
     :display ["Viaje" "Viajes"]
     
     :atts
     {fecha-salida         {:display "Fecha Salida"
                            :type Timestamp
                            :constraints :not-nil} ;; widget :datetime por defecto
      
      fecha-llegada        {:display "Fecha Llegada"
                            :type Timestamp
                            :constraints :not-nil
                            :computable #()}
      
      asientos-disponibles {:display "Asientos disponibles"
                            :type Integer
                            :fn 
                            #(count 
                               (filter 
                                 (fn [seat] (=> seat :seat/disponible?)) seats))}}
     :fns
     {retraso              {:display "Retraso"
                            :type DateTime
                            :fn #(interval (now) fecha-llegada)}}
     
     :refs 
     {origen ;; genera una relationship travel-origen-city ? 
      {:display "Origen"
       :entity city
       :cardi :m-to-o
       :constraints :not-nil
       :widget :combobox}
      
      destino ;; genera una relationship travel-destination-city ? 
      {:display "Destino"
       :entity city
       :cardi :m-to-o
       :constraints :not-nil
       :widget :combobox}
      
      bus
      {:display "Bus"
       :entity bus
       :constraints #()}
      
      tickets
      {:display "Tickets"
       :entity ticket
       :cardi :o-to-m
       :widget {:type :selector 
                :aggs {:actions update}}}}
          
     :operations
     {"Nuevo viaje"
      {:type :tag
       :ops  ["Crear único viaje"]}
      "Buscar Pasaje"
      {:type                :selector
       :search-layout       [origen destino [fecha-salida :interval]] 
       :cols                [origen destino fecha-salida fecha-llegada asientos-disponibles retraso cancelado?]
       :operations          ["Revisar Disponibilidad" "Actualizar datos" "Nuevo viaje"]}
      "Cancelar viaje"
      {:type   :update
       :do     #(=> % cancelado? true)}
      "Revisar Disponibilidad"
      {:type :show
       :layout [origen destino fecha-salida fecha-llegada tickets]}
      "Actualizar datos"
      {:type :form-update
       :layout [[[origen fecha-salida] [destino fecha-llegada]] tickets]
       :operations ["Cancelar Viaje"]}
      "Crear único viaje"
      {:type :create
       :layout [bus driver origen destino fecha-salida fecha-llegada]
       :constraints #(> fecha-salida (now))
       :after [#(create (map (fn [s]
                              (record-map :ticket 
                                {:ticket/disponible? true
                                 :ticket/seat s
                                 :ticket/travel this}))
                         (=> this bus seats)))]}}
     
     :constraints
     [#(and (not= origin destino) (not= fecha-salida fecha-llegada)) ;; opcion 1
      [#(not= origin destino)                                        ;; opcion 2 / activar por eventos de widgets?
       "Ciudad de origen no puede ser igual a la de destino"]])
    
    :scheds
    [{:begin (now) :end (now) :period [1 :week] 
      :fn #()}]
    
    :menu-ops {:type :menubar
               :ops  wa
               {"Ventas pasajes"
                {:type :tag
                 :ops }}})