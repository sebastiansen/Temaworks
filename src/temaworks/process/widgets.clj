(in-ns 'temaworks.process.views)

(defprotocol Widget-wrapper
  (turn-optional-widget [this])
  (constrain-widget [this])
  (empty-value? [this value])
  (gen-rows [this]))

(defn optional-helper
  [wrapper & args]
  (let [{:keys [label enabler]} wrapper
        checkbox (Checkbox. (.getValue label))]
    (apply enabler (cons false (if args args [])))
    (add-event! checkbox "onCheck"
      #(apply enabler (cons (.isChecked checkbox) (if args args []))))
    (merge wrapper
      {:checkbox checkbox
       :disabled? #(not (.isChecked checkbox))})))

(defn constrain-widget-helper
  [wrapper func]
  (let [{:keys [getter widget]} wrapper]
    (assoc 
      wrapper
      :getter
      #(if (empty-value? wrapper (getter))  
         (throw (WrongValueException.
                  (func widget)
                  "No se permite vacío o espacios en blanco. 
                   Debe especificar un valor diferente"))
         (getter)))))

(defn gen-rows-helper
  [{:keys [checkbox label]} widget]
  [(multi-append! (Row.)
     [(if checkbox
        checkbox
        label)
      widget])])

(defrecord Attribute-widget-wrapper [widget-type label widget getter setter enabler unlocker]
  Widget-wrapper
  (turn-optional-widget [this]
    (optional-helper this))
  (constrain-widget [this]
    (constrain-widget-helper this identity))
  (empty-value? [this value] (nil? value))
  (gen-rows [this] (gen-rows-helper this widget)))

(defrecord Reference-widget-wrapper [widget-type label widget getter setter enabler unlocker signal gen-widget]
  Widget-wrapper
  (turn-optional-widget [this]
    (optional-helper this))
  (constrain-widget [this]
    (constrain-widget-helper this last))
  (empty-value? [this value] 
    (or (empty? (children value))
      (distinct? (keys (select-pks (first value)))
        (filter #(or (and (is-type? % Attribute) (:pk? %)) 
                   (and (is-type? % Reference) (:pk? ((:rel %)))))
          (concat (:refs (:entity-type value)) (:atts (:entity-type value)))))))
  (gen-rows [this]
    (if-let [cbox (:checkbox this)]
      (gen-widget cbox)
      (gen-widget))))

(defrecord Many-ref-widget-wrapper [widget-type label widget getter setter enabler unlocker signal gen-widget]
  Widget-wrapper
  (turn-optional-widget [this]
    (optional-helper this))
  (constrain-widget [this])
  (empty-value? [this value])
  (gen-rows [this]
    (if-let [cbox (:checkbox this)]
      (gen-widget cbox)
      (gen-widget))))

(defrecord Interval-widget-wrapper [widget-type label widget getter setter enabler unlocker primitive-wrappers]
  Widget-wrapper
  (turn-optional-widget [this]
    (optional-helper this 2))
  (constrain-widget [this]
    (constrain-widget-helper this first))
  (empty-value? [this value] (some nil? value))
  (gen-rows [this] (gen-rows-helper this (multi-append! (Hlayout.) widget))))

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
  (let [wrapper (gen-basic-att att)]
    (assoc wrapper
      :unlocker #(.setButtonVisible (:widget wrapper) %))))
  
;; This includes {Text | Long | Double | Decimal | Int}box
(defmethod gen-att-widget :default [att]
  (gen-basic-att [att]))

(defmethod gen-att-widget :datetime [att])
  
(defmethod gen-att-widget :radiogroup [att]
  (let [wrapper (gen-generic-att att)
        {:keys [label widget]} wrapper
        aggs (:aggregates att)]
    (.setOrient widget (:orient aggs))
    (multi-append! widget (map #(Radio. %) (:options aggs)))
    (merge wrapper
     {:getter   #(if (nil? (.getSelectedItem widget))        
                   nil
                   (.. widget getSelectedItem getLabel))
      :setter   #(.setSelectedItem widget 
                   (first 
                     (filter (fn [x] (= (.getLabel x) %))
                       (.getItems widget))))
      :enabler  #(doseq [x (.getItems widget)] 
                   (.setDisabled x (not %)))
      :unlocker #(doseq [x (.getItems widget)] 
                   (.setDisabled x (not %)))})))

(defmethod gen-att-widget :combobox [att]
  (let [wrapper (gen-generic-att att)
        widget (:widget wrapper)]
    (doseq [x (:options (:aggregates att))] 
      (.appendItem widget x))
    (doto widget 
      (.appendItem "Seleccionar...")
      (.setReadonly true)
      (.setSelectedIndex 0))
    (merge wrapper
      {:getter   #(if (zero? (.getSelectedIndex widget))
                    nil
                    (.getValue widget))
       :setter   #(.setValue widget %)
       :enabler  #(.setDisabled widget (not %))
       :unlocker #(.setButtonVisible widget %)})))
  
(defmethod gen-att-widget :checkbox [att]
  (let [wrapper (gen-basic-att att)
        widget (:widget wrapper)]
    (merge wrapper
      {:getter   #(.isChecked widget)
       :setter   #(.setChecked widget %)
       :enabler  #(.setDisabled widget (not %))
       :unlocker #(.setDisabled widget (not %))})))
  
(defmethod gen-att-widget :filebox [att]
  (let [wrapper (gen-basic-att att)
        media (ref nil)
        file-name (doto (Textbox.) (.setReadonly true))
        down-button (doto (Button. "Descargar") (.setDisabled true) (add-event! "onClick" #(Filedownload/save @media)))
        setter (fn [media-val media-name down-disabled?]
                 (dosync (ref-set media media-val))
                 (.setValue file-name media-name)
                 (.setDisabled down-button down-disabled?))
        up-button (doto (Button. "Examinar...") (.setUpload "true")
                    (.addEventListener "onUpload"
                      (proxy [EventListener][]
                        (onEvent [e]
                          (let [media (.getMedia e)]
                            (setter media (.getName media) false))))))
        remove-button (doto (Button. "Vaciar") (add-event! "onClick" #(setter nil nil true)))]
    (multi-append! (:widget wrapper)
      [file-name up-button down-button remove-button])
    (merge wrapper
      {:getter   #(@media)
       :setter   (fn [file-path]
                   (setter media (if media (.getName media)) false))
       :enabler  #(doseq [button [down-button remove-button up-button]] (.setDisabled button true))
       :unlocker #(.setReadonly up-button (not %))})))

(defmulti gen-ref-widget (fn [refe scope] (:widget-type refe)))

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
             ref-label
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
               ([](generator c-wrappers))
               ([checkbox]
                 [(multi-append! (Row.)
                    [checkbox
                     (gen-grid 
                       (let [rows (generator c-wrappers)]
                         (if (> (count rows) 1)
                           (rest rows)
                           rows)) 
                       "18%")])])))))
       
       (clear-combo [c-wrapper]
         (.clear (cast ListModelList (.getModel (:combo c-wrapper)))))
  
       (load-combo [{:keys [entity-type combo model]}
                    record-maps]
         (dosync (ref-set model record-maps))
         (.addAll (cast ListModelList (.getModel combo)) 
           (reduce
             #(do (.add %1 %2) %1)
             (ArrayList.)
             (conj 
               (map #(to-str %) record-maps)
               "Seleccionar..."))))
  
       (selected [{:keys [entity-type combo model]}]
         (let [idx (.getSelectedIndex combo)]
           (if (zero? idx)
             (record-map entity-type)
             (nth @model (dec idx)))))
       
       (set-value [{:keys [combo model]} record-map]
         (.setSelectedIndex combo 
           (inc (first (positions (fn [x] (is? x record-map)) @model)))))
  
       (make-cascade-combos
         [c-wrappers]
         
         (defn- load-children
           [c-wrappers r-map with-cleaning?]
           (if-not (empty? (children r-map))
             (reduce 
               (fn [r-map c-wrapper]
                 (let [{:keys [filter-ref entity-type combo]} c-wrapper
                       new-map (record-map entity-type filter-ref (with-meta (children r-map) {:fuzzy false}))]
                   (when with-cleaning? (clear-listeners! combo "onAfterRender"))
                   (clear-combo c-wrapper)
                   (load-combo c-wrapper (map #(hack-map->record-map entity-type %) 
                                           @(make-joins (record-map->table new-map) new-map)))
                   new-map))
               r-map
               (rest c-wrappers))
             (doseq [{:keys [combo]} (rest c-wrappers)]
               (.setSelectedIndex combo 0))))
      
         (defn- set-parents
           [c-wrappers r-map]
           (when-not (empty? (children r-map))
             (loop [reference (:filter-ref (first c-wrappers))
                    parents (rest c-wrappers)
                    filtered-map r-map]
               (when-not (empty? parents)
                 (let [{:keys [filter-ref entity-type combo]} (first parents)]
                   (when (zero? (.getSelectedIndex combo))
                     (let [this-map (get (children filtered-map) reference)]
                       (set-value (first parents))
                       (recur filter-ref (rest parents) this-map))))))))
         
         (defn- add-events
           []
           (let [total (count c-wrappers)]
             (doseq [idx (range total)]
               (let [c-wrapper (nth c-wrappers idx)]
                 (add-event! (:combo c-wrapper) "onSelect"
                   #(let [record-map (selected c-wrapper)]
                      (load-children (subvec c-wrappers idx) record-map true)
                      (set-parents (vec (reverse (subvec c-wrappers (- (dec total) idx)))) record-map)))))))
           
         (defn- getter
           []
           (let [c-wrapper (first c-wrappers)] 
             (reduce 
               (fn [[reference record-map] c-wrapper]
                 (assoc-val record-map reference (selected c-wrapper)))
               [(:filter-ref c-wrapper) (selected c-wrapper)]
               (reverse c-wrappers))))
         
         (defn- setter
           [rec-map]
           (if (nil? rec-map)
             (load-children c-wrappers (record-map (:entity-type (first c-wrappers))) false)
             (reduce
               (fn [[reference cascade-setter filtered-map] c-wrapper]
                 (let [{:keys [filter-ref entity-type combo]} c-wrapper
                       this-map (get (children filtered-map) reference)]
                   (if (= c-wrapper (first c-wrappers))
                     (do
                       (add-event! combo "onAfterRender"
                         (fn [] (do
                                  (set-value c-wrapper this-map)
                                  (load-children c-wrappers this-map false)
                                  (cascade-setter))))
                       [reference setter filtered-map])
                     [filter-ref
                      #(add-event! combo "onAfterRender"
                         (fn [] (do
                                  (set-value c-wrapper this-map)
                                  (cascade-setter))))
                      this-map])))
               [refe
                #() 
                {:values {refe rec-map}}]
               (reverse c-wrappers))))
         
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
                 (fn [record-map]
                   (doto combo 
                     (clear-listeners! "onAfterRender")
                     (add-event! "onAfterRender" #(set-value c-wrapper record-map)))))))
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

(defmethod gen-ref-widget :button [refe scope])

(defmethod gen-ref-widget :checkgroup [refe scope]
  (let [rel ((:rel refe))
        mapping-entity ((:from-entity rel))
        mapping-rel ((:mapping-rel  rel))
        from-ref (:from-ref mapping-rel)
        to-ref (:to-ref mapping-rel)
        rec-map (ref nil)]
    (defn- gen-checkgroup [from-map]
      (let [example-map (record-map mapping-entity from-ref from-map)
            mapping-maps (map hack-map->record-map @(make-joins (record-map->table example-map) example-map))
            check-map (ref (reduce 
                             (fn [check-map to-map]
                               (let [mapped-val (first (filter #(is? to-map (get (children %) to-ref)) mapping-maps))]
                                 (assoc check-map
                                   (doto (Checkbox. (to-str to-map))
                                     (.setChecked (not (nil? mapped-val))))
                                   [to-map mapped-val])))
                             {}
                             (search-all ((:to-entity ((:rel to-ref)))))))]
        (doseq [checkbox (vals @check-map)]
          (add-event! checkbox "onCheck"
            #(if (.isChecked checkbox)
               (let [[to-map mapped-val] (@check-map checkbox)
                     new-val (assoc-val (record-map mapping-entity from-ref from-map) to-ref to-map)]
                 (create new-val)
                 (dosync (assoc checkbox [to-map new-val]))
               (let [[to-map old-val] (@check-map checkbox)]
                 (delete old-val)
                 (dosync (assoc checkbox [to-map nil])))))))
        (multi-append! (Vlayout.) (vals @check-map))))
    
    (Many-ref-widget-wrapper.
      :selector 
      []
      []
      (fn [] )
      (fn [r-map] (dosync (ref-set rec-map r-map)))
      (fn [b] )
      (fn [b] )
      (fn [t n o])
      (fn 
        ([] 
          (multi-append! (Row.)
            [(Label. (str (:ref-name refe) ":"))
             (gen-checkgroup @rec-map)]))))))

(defmethod gen-ref-widget :list [refe sope])

(defn gen-interval-widget [{:keys [att]}]
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
         [(Label. "Desde: ") (:widget from-tuple)])
       (doto (Space.) (.setSpacing "20px"))
       (multi-append!
         (Hlayout.)
         [(Label. "Hasta: ") (:widget to-tuple)])]
      #(sort (map (fn [t] ((:getter t))) [from-tuple to-tuple]))
      (modifier :setter)
      (modifier :enabler)
      (modifier :unlocker)
      [from-tuple
       to-tuple])))

(def specs-map 
  {:optional turn-optional-widget
   :constrain constrain-widget})

(defn comp-wrapper
  [wrapper specs]
  (if (empty? specs)
    wrapper
    ((apply comp (map specs-map specs)) wrapper)))

(derive Reference ::ref-type)
(derive Many-ref ::ref-type)

(defmulti gen-widget
  "Constructs a new widget wrapper from an Attribute, Reference or Interval instance."
   class)

(defmethod gen-widget Attribute [att]
  (comp-wrapper (gen-att-widget att) (:specs (meta att))))

(defmethod gen-widget Interval [interval]
  (comp-wrapper (gen-interval-widget interval) (:specs (meta interval))))

(defmethod gen-widget ::ref-type [refe]
  (let [{:keys [specs scope]} (meta refe)]
    (comp-wrapper (gen-ref-widget refe scope) specs)))

(defn make-example
  ([entity-type widgets]
    (reduce
      (fn [widgets [att-ref wrapper]]
        (assoc-val widgets
          att-ref
          ((:getter wrapper))))
      (record-map entity-type))
    widgets))

(defn make-widgets
  "Constructs a hash-map with 
   val -> Attribute | Reference | Interval
   key -> *Widget-wrapper"
  ([order scope]
    (make-widgets order scope nil nil (:specs (meta order))))
  
  ([order scope reference ref-map]
    (make-widgets order scope reference ref-map (:specs (meta order))))
  
  ([order scope reference ref-map specs]
    (defn- new-order
      [order]
      (map #(with-meta %
              {:specs (concat (:specs (meta %)) specs)
               :scope scope}) order))
    
    (defn- maker-helper
      [order]
        (reduce 
          #(assoc %1
             %2 (gen-widget %2))
          {}
          order))
    
    (let [widgets (maker-helper (new-order order))]
      (if reference
        (assoc widgets
          reference
          (let [wrapper (gen-widget (with-meta (assoc reference :widget-type :ref-box) {:scope scope :specs specs}))
                {:keys [widget setter enabler unlocker checkbox]} wrapper]
            (setter ref-map)
            (enabler true)
            (unlocker false)
            (when checkbox
              (doto checkbox
                (.setDisabled true)
                (.setChecked true)))
            wrapper)) 
        widgets))))

(defn make-rows
  [wrappers]
  (reduce 
    (fn [wrappers wrapper] 
      (concat wrappers (gen-rows wrapper)))   
    []
    wrappers))

(defn set-widgets-values
  [widgets r-map]
  (doseq [[att-ref wrapper] widgets]
    ((:setter wrapper) 
      (get (children r-map) att-ref))))

(defn gen-quantity-widget
  [{:keys [multi-name]}]
  (let [widget (Intbox.)]
    (Attribute-widget-wrapper.
      :intbox
      (Label. (str "# de " multi-name " a crear:"))
      widget
      #(.getValue widget)
      #(.setValue widget %)
      #(.setDisabled widget (not %))
      #(.setReadonly widget (not %)))))

(defn restart-widgets
  [wrappers]
  (doseq [wrapper wrappers]
    (let [{:keys [checkbox enabler]} wrapper]
      (.setChecked checkbox false)
      (if (is-type? wrapper Interval-widget-wrapper)
        (do (enabler false 2)
          (setter nil 2))
        (do (enabler false)
          (setter nil))))))