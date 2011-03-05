(in-ns 'temaworks.process.views)

;; Wrappers definition

tema-widget

tema-widget -> zk-widget

extends?

{Textbox Generic-widget
 Intbox Generic-widget}

(defrecord Generic-Form-widget [at-ref]
  )

(defprotocol Widget
  (gen-widget  []                         )
  (getter      [this]                     ) 
  (setter      [this cloud-map]           ) 
  (enabler     [this value]     "Enables ") 
  (unlocker    [this value]     "Unlock " ))

(defprotocol Form-widget
  (make-rows [this rows]))

(defprotocol )

(defprotocol Interval-widget-wrapper
  [primitive-wrappers])

(defprotocol Constrained-widget-wrapper
  [primitive-wrappers])

(defrecord Attribute-widget-wrapper [att]
  ;;Widget-wrapper
  )

(defrecord Reference-widget-wrapper
  [widget-type label widget getter setter enabler unlocker signal gen-widget])


(defmulti to-str class)
(defmethod to-str Long           [x] (Long/toString x)             )
(defmethod to-str Integer        [x] (Integer/toString x)          )
(defmethod to-str Double         [x] (format "%.2f" (double x))    )
(defmethod to-str Float          [x] (Float/toString x)            )
(defmethod to-str java.util.Date [x] (format "%1$td-%1$tm-%1$tY" x))
(defmethod to-str String         [x] x                             )
(defmethod to-str Boolean        [x] (if x "Sí" "No")              )
;;CLOUD MAP
(defmethod to-str :default       [x] (String/valueOf x)            )

(defmulti gen-att-widget
  "Generates Attribute-widget-wrapper"
  :widget-type)

(defn gen-generic-att
  [att]
  (Attribute-widget-wrapper.
    (:widget-type att)
    (Label. (str (:att-name att) ":"))
    (to-class ((:widget-type att) widget-types))
    nil nil nil nil))

(defn gen-basic-att 
  [att]
  (let [wrapper (gen-generic-att att)
        {:keys [widget]} wrapper]
    (merge wrapper 
      {:getter   #(.getValue widget)
       :setter   #(.setValue widget %)
       :enabler  #(.setDisabled widget (not %))
       :unlocker #(.setReadonly widget (not %))})))

(defmethod gen-att-widget :textbox [att]
  (let [wrapper (gen-basic-att att)
        {:keys [widget]} wrapper
        aggs (:aggregates att)]
    (assoc wrapper
      :widget
      (if aggs
        (case (:length aggs)
          :long
          (doto widget (.setWidth 300))
          :short
          (doto widget (.setWidth widget 30)))
        widget))))
  
(defmethod gen-att-widget :textarea [att]
  (let [wrapper (gen-basic-att att)
        {:keys [widget]} wrapper]
    (assoc wrapper
      :widget
      (doto widget
        (.setRows widget 5)
        (.setWidth widget "500px")))))
  
(defmethod gen-att-widget :datebox [att]
  (let [wrapper (gen-generic-att att)
        {:keys [widget]} wrapper]
    (assoc wrapper
      :unlocker #(.setButtonVisible widget %))))
  
;; This includes {Text | Long | Double | Decimal | Int}box
(defmethod gen-att-widget :default [att]
  (gen-basic-att [att]))

(defmethod gen-att-widget :datetime [att])
  
(defmethod gen-att-widget :radiogroup [att]
  (let [wrapper (gen-att-helper)] 
    (.setOrient widget (:orient (:aggregates att)))
    (doseq [x (:options (:aggregates att))] 
      (cascade-append! [(Radio. x) widget]))
    
      widget
      #(if (nil? (.getSelectedItem widget))        
         nil
         (.. widget getSelectedItem get (make-att-label att)))
      #(.setSelectedItem widget (first 
                                  (filter (fn [x] (= (.get(make-att-label att) x) %))
                                    (.getItems widget))))
      #(doseq [x (.getItems widget)] 
         (.setDisabled x (not %)))
      #(doseq [x (.getItems widget)] 
         (.setDisabled x (not %)))))

(defmethod gen-att-widget :combobox [att]
  (let [widget (make-widget att)]
    (.appendItem widget "Seleccionar...")
    (doseq [x (:options (:aggregates att))] 
      (.appendItem widget x))
    (.setReadonly widget true)
    (.setSelectedIndex widget 0)
    (Attribute-widget-wrapper.
      (:widget-type att)
      (make-att-label att)
      widget
      #(if (zero? (.getSelectedIndex widget))
          nil
          (.getValue widget))
      #(.setValue widget %)
      #(.setDisabled widget (not %))
      #(.setButtonVisible widget %))))

(defmethod gen-att-widget :checkbox [att]
  (let [widget (make-widget att)]
    (Attribute-widget-wrapper.
      (:widget-type att)
      (make-att-label att)
      widget
      #(.isChecked widget)
      #(.setChecked widget %)
      #(.setDisabled widget (not %))
      #(.setDisabled widget (not %)))))
  
(defmethod gen-att-widget :file [att]
  (let [box (Hbox.)
        file-box (doto (Textbox.) (.setReadonly true))
        up-button (Button. "Examinar...")
        down-button (Button. "Descargar")
        remove-button (Button. "Eliminar")
        file (ref nil)]
    (cascade-append!
      [up-button box]
      [down-button box]
      [remove-button box])
    ;; TODO: getter, setter
    
     
    (Attribute-widget-wrapper.
      (:widget-type att)
      (make-att-label att)
      box
      (fn [] ())
      (fn [] ())
      #()
      #(.setReadonly up-button (not %)))))

(defmulti gen-ref-widget (fn [refe scope] (:widget-type refe)))

;; MISMA WIDGET QUE PARA BUSQUEDA AVANZADA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;(defmethod gen-ref-widget :ref-form [refe]
;;  )

;; Signals are missing.
(defmethod gen-ref-widget :ref-box [refe scope]
  (let [ref-box (doto (Textbox.) (.setReadonly true))
        rel ((:rel refe))
        select-button (doto (Button. "Seleccionar") (.setImage (:up icons)))
        edit-button (doto (Button. "Editar") (.setImage (:edit icons)))
        to-entity ((:to-entity rel))
        reference (ref {to-entity {}})
        setter #(dosync 
                  (ref-set reference %)
                  (.setValue ref-box ((:to-str to-entity) %))
                  (.setDisabled edit-button false))
        box (Hbox.)
        row (Row.)
        label (Label. (str (:single-name to-entity) ":"))]
    
    (multi-append! box
         [ref-box edit-button select-button])
    
    (add-event! select-button "onClick"
      #(::gen-ref-selector to-entity setter (:ref-name refe) (.getPage box) scope))
    
    (add-event! edit-button "onClick"
      #(apply (:gen-view scope) (::gen-form to-entity @reference scope)))
    
    (.setDisabled edit-button true)
       
    ;;return value
    (Reference-widget-wrapper. 
      (:widget-type refe)
      [label]
      [box]
      #(@reference)
      setter
      #(doseq [x [ref-box select-button edit-button]] 
         (.setDisabled x (not %)))
      #(.setDisabled select-button (not %))
      
      (fn [entity-type old-record new-record]
        ())
      (fn
        ([]
          [(multi-append!
            (Row.)
            [label box])])
        ([checkbox]
          [(multi-append!
            (Row.)
              [checkbox box])])))))
  
(defmethod gen-ref-widget :combobox [refe scope]
  (let [ref-label (Label. (str (:ref-name refe) ":"))]
    (letfn 
      [(clear-listeners! [combo name]
         (loop [iterator (.getListenerIterator combo name)]
           (when (.hasNext iterator)
             (.next iterator)
             (.remove iterator)
             (recur iterator))))
       
       (make-c-wrapper
         [reference]
         (let [to-entity ((:to-entity ((:rel reference))))]
           {:filter-ref (:filter-ref to-entity)
            :entity-type to-entity
            :combo (doto (Combobox.) 
                     (.setReadonly true) 
                     (.setModel (ListModelList.)))
            :model (ref [])
            :label (Label. (str (:single-name to-entity) ":"))}))
    
       (generator 
         [c-wrappers]
         (let [rows (map #(multi-append! (Row.)
                            [%1 %2])
                      (map :label c-wrappers) 
                      (map :combo c-wrappers))] 
           (if (> (count (map :combo c-wrappers)) 1)
             (cons
               (doto (Row.) 
                 (.appendChild (doto ref-label 
                                 ;;(comment "weas de style")
                                 )))
               rows)
             rows)))
  
       (make-wrapper
         [c-wrappers getter setter]
         (do 
           (doseq [{:keys [combo]} c-wrappers]
             (add-event! combo 
               "onAfterRender"
               #(.setSelectedIndex combo 0)))
           (Reference-widget-wrapper.
             :combobox
             [ref-label]
             (map :combo c-wrappers)
             getter
             setter 
             #(doseq [{:keys [combo]} c-wrappers]
                (.setDisabled combo (not %)))
             #(doseq [{:keys [combo]} c-wrappers]
                (.setButtonVisible combo %))
             (fn [entity-type old-record new-record]
               (let [c-wrapper (last c-wrappers)]
                 (when (is-type? entity-type (:entity-type c-wrapper))
                   (if (= old-record (selected c-wrapper))
                     (setter new-record)
                     (do
                       (clear-combo c-wrapper)
                       (setter (selected c-wrapper)))))))
             (fn 
               ([]
                 (generator c-wrappers))
               ([checkbox]
                 [(multi-append! (Row.)
                    [checkbox
                     (::gen-grid 
                       (let [rows (generator c-wrappers)]
                         (if (> (count rows) 1)
                           (rest rows)
                           rows)) 
                       "18%")
])])))))
       
       (clear-combo [{:keys [combo]}]
         (.clear (cast ListModelList (.getModel combo))))
  
       (load-combo [{:keys [entity-type combo model]}
                    cloud-maps]
         (dosync (ref-set model cloud-maps))
         (.addAll (cast ListModelList (.getModel combo)) 
           (reduce
             #(do (.add %1 %2) %1)
             (ArrayList.)
             (conj 
               (map #((:to-str entity-type) %) cloud-maps)
               "Seleccionar..."))))
  
       (selected [{:keys [entity-type combo model]}]
         (let [idx (.getSelectedIndex combo)]
           (if (zero? idx)
             {entity-type nil}
             (nth @model (dec idx)))))
       
       (set-value [{:keys [combo model]} cloud-map]
         (.setSelectedIndex combo 
           (inc (first (positions (fn [x] (is? x cloud-map)) @model)))))
  
       (make-cascade-combos
         [c-wrappers]
         
         (defn- load-children
           [c-wrappers cloud-map with-cleaning?]
           (if-not (empty? (children cloud-map))
             (reduce 
               (fn [cloud-map c-wrapper]
                 (let [{:keys [filter-ref entity-type combo]} c-wrapper
                       new-map {entity-type {filter-ref (with-meta (children cloud-map) {:fuzzy false})}}]
                   ;;(add-event! combo "onAfterRender" 
                   ;;  (fn []
                   ;;    (do
                   (when with-cleaning? (clear-listeners! combo "onAfterRender"))
                   (clear-combo c-wrapper)
                   (load-combo c-wrapper @(make-joins (cloud-map->table new-map) new-map))
                   ;;)))
                   new-map))
               cloud-map
               (rest c-wrappers))
             (doseq [{:keys [combo]} (rest c-wrappers)]
               (.setSelectedIndex combo 0))))
      
         (defn- set-parents
           [c-wrappers cloud-map]
           (loop [reference (:filter-ref (first c-wrappers))
                  parents (rest c-wrappers)
                  filtered-map cloud-map]
               (when-not (empty? parents)
                 (let [{:keys [filter-ref entity-type combo]} (first parents)]
                   (when (zero? (.getSelectedIndex combo))
                     (let [this-map {entity-type (get (children filtered-map) reference)}]
                       (set-value (first parents))
                       (recur filter-ref (rest parents) this-map)))))))
         
         (defn- add-events
           []
           (let [total (dec (count c-wrappers))]
             (doseq [idx (range (count c-wrappers))]
               (let [c-wrapper (nth c-wrappers idx)
                     {:keys [combo]} c-wrapper]
                 (add-event! combo "onSelect"
                   #(let [cloud-map (selected c-wrapper)] 
                      (dbg cloud-map)
                      (load-children (subvec c-wrappers idx) cloud-map true)
                      (set-parents (vec (reverse (subvec c-wrappers (- total idx)))) cloud-map)))))))
           
         ;; POSIBLE CHANGES DUE TO NEW CLOUD-MAP STRUCTURE 
         (defn- getter
           []
           (let [{:keys [filter-ref entity-type]} (first c-wrappers)] 
             (reduce 
               (fn [[reference cloud-map] c-wrapper]
                 {entity-type (assoc (children cloud-map) reference (selected c-wrapper))}))
             [(:filter-ref (first c-wrappers)) (selected (first c-wrappers))]
             (reverse c-wrappers)))
         
         (defn- setter
           [cloud-map]
           (reduce
             (fn [[reference setter filtered-map] c-wrapper]
               (let [{:keys [filter-ref entity-type combo]} c-wrapper
                     this-map {entity-type (get (children filtered-map) reference)}]
                 (if (= c-wrapper (last c-wrappers))
                   (do
                     (add-event! combo "onAfterRender"
                       (fn [] (do
                                (set-value c-wrapper this-map)
                                (setter)
                                (load-children c-wrappers cloud-map))))
                     [reference setter filtered-map])
                   [filter-ref
                    #(add-event! combo "onAfterRender"
                       (fn [] (do
                                (set-value c-wrapper this-map)
                                (setter))))
                    this-map])))
             [(:filter-ref (first (reverse c-wrappers))) #() cloud-map]
             (reverse (rest c-wrappers))))
         
         (add-events)
         (make-wrapper
           c-wrappers
           getter
           setter))
       
       (make-combos
         ([]
           (defn- maker
             [c-wrappers]
             (let [c-wrapper (first c-wrappers)
                   {:keys [combo]} c-wrapper]
               (make-wrapper
                 [c-wrapper]
                 #(selected c-wrapper)
                 (fn [cloud-map]
                   (doto combo 
                     (clear-listeners! "onAfterRender")
                     (add-event! "onAfterRender" #(set-value c-wrapper cloud-map)))))))
           (make-combos maker [(make-c-wrapper refe)]))
         ([maker c-wrappers]
           (let [{:keys [filter-ref entity-type]} (first c-wrappers)]
             (if filter-ref
               (do
                 (load-combo (first c-wrappers) [])
                 (make-combos make-cascade-combos (vec (cons (make-c-wrapper filter-ref) c-wrappers))))
               (do 
                 (load-combo (first c-wrappers) (search-all entity-type))
                 (maker c-wrappers))))))]
      (make-combos))))
      
(defmulti gen-many-widget :widget-type) 

(defmethod gen-many-widget :selector
  [refe scope cloud-map-ref]
  (let [from-entity ((:from-entity ((:rel refe))))
        selector nil 
;(gen-selector from-entity 
;                   (:mutual-ref refe) 
;                   cloud-map-ref
;                   scope)
]
     
    (Reference-widget-wrapper.
      :selector 
      [(Label. (str (:ref-name refe) ":"))]
      [(doto 
         ;;layout
         (first selector) 
         (.setWidth "500px") 
         (.setHeight "500px"))]
      (fn [] )
      (fn [r] )
      (fn [b] )
      (fn [b] )
      (second selector)
      (fn []))))

(comment (:selector-form
            ()
            :button
            ()
            ;; many-to-many
            :checkgroup
            ()
            :list
            ()))

(defn gen-interval-widget [att]
  (let [from-tuple (gen-att-widget att)
        to-tuple (gen-att-widget att)
        ;; MultiStateModifierStrategyFactory
        modifier (fn [op] 
                    (fn [value option] 
                      (defn- setter [t] (get t op))
                      (case option
                        0
                        ((setter from-tuple) value)
                        1
                        ((setter to-tuple) value)
                        2
                        (dorun (map #((setter %) value) [from-tuple to-tuple]))
                      
                        (throw (IllegalArgumentException. "Option must be either 0, 1 or 2")))))]
     
     (Interval-widget-wrapper.
       (:widget-type att)
       (:label from-tuple)
       [(multi-append!
          (Hlayout.)
          [(Label. "Desde") (:widget from-tuple)]) 
        (multi-append!
          (Hlayout.)
          [(Label. "Hasta") (:widget to-tuple)])]
       #(sort (map (fn [t] ((:getter t))) [from-tuple to-tuple]))
       (modifier :setter)
       (modifier :enabler)
       (modifier :unlocker)
       [from-tuple
        to-tuple])))

(defmulti gen-widget (fn [att-ref scope] (type att-ref)))
(defmethod gen-widget Attribute [att scope]
  (gen-att-widget att))
(defmethod gen-widget Reference [refe scope]
  (gen-ref-widget refe scope))

(defmulti empty-value? (fn [wrapper value] (type wrapper)))
(defmethod empty-value? Attribute-widget-wrapper [wrapper value]
  (nil? value))

(defmethod empty-value? Reference-widget-wrapper [wrapper value]
  (or (empty? (children value))
    (distinct? (keys (select-pks (first value)))
      (filter #(or (and (is-type? % Attribute) (:pk? %)) 
                 (and (is-type? % Reference) (:pk? ((:rel %)))))
        (concat (:refs (root value)) (:atts (root value)))))))

(defmethod empty-value? Interval-widget-wrapper [wrapper value]
  (some nil? value))

(defmulti constrain-widget type)

(defmethod constrain-widget Interval-widget-wrapper [wrapper]
  (let [{:keys [getter widget]} wrapper]
    (assoc 
      wrapper
      :getter
      #(if (empty-value? wrapper (getter))  
         (throw (WrongValueException.
                  (first widget)
                  "No se permite vacío o espacios en blanco. Debe especificar un valor diferente"))
         (getter)))))

(defmethod constrain-widget Attribute-widget-wrapper)

(defmethod constrain-widget :default [wrapper]
  (let [{:keys [widget-type getter widget]} wrapper] 
    (assoc 
      wrapper
      :getter
      #(let [value (getter)
             exception-widget (if (is-type? wrapper Reference-widget-wrapper)
                                (last widget)
                                widget)]
         (if (empty-value? value wrapper)
           (throw (WrongValueException.
                    exception-widget
                    "No se permite vacío o espacios en blanco. Debe especificar un valor diferente"))
           value)))))

(defmulti turn-optional-widget type [wrapper]
  "Constructs a new widget wrapper that only yields the widget's value when its
  matching checkbox is checked.")

(defmethod turn-optional-widget type [wrapper]

  (let [checkbox (Checkbox. (.getValue (if (is-type? wrapper Reference-widget-wrapper)
                                         (last (:label wrapper))
                                         (:label wrapper))))
        getter (:getter wrapper)]
    (add-event! checkbox "onCheck"
      (if (is-type? wrapper Interval-widget-wrapper)
        #((:enabler wrapper) (.isChecked checkbox) 2)
        #((:enabler wrapper) (.isChecked checkbox))))
    (if (is-type? wrapper  Interval-widget-wrapper)
      ((:enabler wrapper) false 2)
      ((:enabler wrapper) false))
    (merge wrapper
      {:checkbox checkbox
       :disabled? #(not (.isChecked checkbox))})))

(defn comp-makers
  [makers-map specs]
  (let [corrected-specs (let [unique-specs (distinct specs)]
                          (if-not (or (contains? (set unique-specs) :simple) 
                                    (contains? (set unique-specs) :interval))
                            (conj unique-specs :simple)
                            unique-specs))
        makers (reduce 
                 #(if (contains? (set corrected-specs) (key %2))
                    (conj %1 (val %2))
                    %1)
                 []
                 makers-map)]
    (apply comp makers)))

(defn make-example
  ([entity-type widgets]
    (make-example entity-type widgets [:simple]))
  ([entity-type widgets specs]
    (defn- simple-func
      [example [att-ref wrapper]]
      {entity-type
       (assoc (children example) 
         att-ref  
         (let [value ((:getter wrapper))]
           (if (is-type? att-ref Attribute)
             value
             (children value))))})
    (defn- interval-func
      [[from-map to-map] [att-ref wrapper]]
      (let [value ((:getter wrapper))
            att-ref-assoc #({entity-type
                             (assoc (children %1) 
                               att-ref %2)})]
        (if (is-type? wrapper Interval-widget-wrapper)
          (map att-ref-assoc
            [from-map to-map] value)         
          (vector
            (att-ref-assoc from-map value)
            to-map))))
    (defn- optional-func
      [func]
      (fn [example widget]
        (if ((:disabled? (val widget)))
          example
          (func example widget))))
    (reduce
      (comp-makers 
        (array-map 
          :simple simple-func
          :optional optional-func
          :interval interval-func)
        specs)
      (if (contains? (set specs) :interval)
        [{entity-type {}} {entity-type {}}]
        {entity-type {}})
      widgets)))

(defn make-widgets
  "specs: :simple :optional :interval :constrain"
  ([order scope]
    (make-widgets order scope [:simple]))
  
  ([order scope specs]
    
    (defn- simple-maker
      ([att-ref]
        (simple-maker gen-att-widget att-ref))
      ([func att-ref]
        {att-ref
         (if (is-type? att-ref Attribute)
           (func att-ref)
           (gen-ref-widget att-ref scope))}))
    
    (defn- interval-maker
      [[att-ref is-interval?]]
      (simple-maker #(if is-interval?
                       (gen-interval-widget %)
                       (gen-att-widget %)) att-ref))
    
    (defn- optional-maker
      [widget]
      (let [[att-ref wrapper] (first widget)]
        {att-ref (turn-optional-widget wrapper)}))
      
    (defn- constrain-maker
      [widget]
      (let [[att-ref wrapper] (first widget)]
        {att-ref (constrain-widget wrapper)}))
    
    (defn- maker
      [func]
      (reduce
        (fn [widgets widget]
          (merge widgets
            (func widget)))
        {}
        order))
    (maker
      (comp-makers 
        (array-map 
          :constrain constrain-maker
          :optional optional-maker
          :interval interval-maker
          :simple simple-maker)
        specs))))
;;reference
(comment
  (if list-selector? 
    (assoc  
      reference
      (let [widget (turn-optional-widget 
                     (gen-ref-widget 
                       (assoc reference 
                         :widget-type 
                         :ref-box)
                       scope))
            checkbox (:checkbox widget)]
        ((:setter widget) ref-map)
        (assoc widget
          (doto checkbox
            (.setDisabled true)
            (.setChecked true))))) 
    (make-widgets (:search-order entity-type) scope [:optional :interval])))

(defn make-rows
  [wrappers]
  (reduce 
    (fn [wrappers wrapper]
      (concat wrappers
        (if (or 
              (is-type? wrapper Attribute-widget-wrapper)
              (is-type? wrapper Interval-widget-wrapper))
          [(multi-append! (Row.)
             [(if-let [cbox (:checkbox wrapper)] cbox
                (:label wrapper))
              (if (is-type? wrapper Interval-widget-wrapper)
                (multi-append! (Hlayout.) (:widget wrapper))
                (:widget wrapper))])]
          (if-let [cbox (:checkbox wrapper)]
            ((:gen-widget wrapper) cbox)
            ((:gen-widget wrapper))))))
    []
    wrappers))

(defn set-widgets-values
  [widgets cloud-map]
  (doseq [[att-ref wrapper] widgets]
    ((:setter wrapper) 
      (if (is-type? att-ref Attribute)
        (get (children cloud-map) att-ref)
        (let [rel ((:rel att-ref))]
          (hash-map ((:to-entity rel))
            (get (children cloud-map) att-ref)))))))

(defn gen-quantity-widget
  [{:keys [multi-name]}]
  (let [widget (Textbox.)]
    (Attribute-widget-wrapper.
      :textbox
      (Label. (str "# de " multi-name " a crear:"))
      widget
      #(.getValue widget)
      #(.setValue widget %)
      #(.setDisabled widget (not %))
      #(.setReadonly widget (not %)))))

(defn restart-widgets
  [wrappers]
  (doseq [wrapper wrappers]
    (.setChecked (:checkbox wrapper) false)
    (if (is-type? wrapper Interval-widget-wrapper)
      (do ((:enabler wrapper) false 2)
        ((:setter wrapper) nil 2))
      (do ((:enabler wrapper) false)
        ((:setter wrapper) nil)))))