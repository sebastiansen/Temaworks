(ns temaworks.process.views
  (:import
    (org.zkoss.zul
      Window
      Groupbox
      Bandbox
      Listbox Listhead Listheader Listitem
      Paging
      Panel Panelchildren
      Vbox Hbox Vlayout Hlayout
      Borderlayout Center West North South
      Grid Row Rows Columns Column
      Label Textbox Intbox Longbox Doublebox Timebox Decimalbox Spinner
      Radio Radiogroup
      Menu Menubar Menupopup Menuitem Messagebox
      Tab Tabbox Tabs Tabpanel Tabpanels
      Toolbar Toolbarbutton
      Caption
      Combobox Checkbox Comboitem
      Button
      Tree Treechildren Treeitem Treerow Treecell
      Listcell
      SimpleListModel
      ListModelList
      ListitemRenderer
      Space
      Filedownload)
    (java.util ArrayList)
    (org.zkoss.zk.ui.event EventListener Event Events InputEvent MouseEvent)
    (org.zkoss.zk.ui WrongValueException Executions)
    (org.zkoss.util.resource Labels)
    (org.zkoss.util.media Media)
    [temaworks.meta.types Entity-type Attribute Reference Relationship Interval Many-ref Tag App Operation View])
  (:use
    [clojure.contrib.seq-utils :only (positions)]
    [clojure.set] 
    [temaworks.handling aspect crud recordmap]
    [temaworks.meta types]
    [temaworks.process.patterns]))

(load "widgets")

(declare gen-ref-selector gen-form gen-selector gen-bulk-create gen-bulk-update gen-option operations-map)

(defn- gen-branch
  [{:keys [name ops icon tooltip]} menu-type scope]
  (let [branch (doto (case menu-type :menubar (Menu. name) :tree (Treeitem. name))
                 (#(when icon (.setImage % (icon icons))))
                 (.setTooltiptext tooltip))]
    (if ops
      (append! branch
        (multi-append! (case menu-type :menubar (Menupopup.) :tree (Treechildren.)) 
          (map (fn [op] (gen-option op menu-type scope)) ops)))
      branch)))

(defmulti gen-option (fn [option menu-type scope] (type option)))

(defmethod gen-option Entity-type [{:keys [multi-name ops icon tooltip]} menu-type scope] 
  (gen-branch {:name multi-name :ops ops :icon icon :tooltip tooltip} menu-type scope))

(defmethod gen-option Operation [{:keys [name icon tooltip entity-type operation]} menu-type scope]
  (doto 
    (case menu-type 
      :menubar 
      (Menuitem. name) 
      :tree 
      (cascade-append! 
        [(doto (Treecell. name)
           (#(when icon (.setImage % (icon icons)))))
         (Treerow.) 
         (Treeitem.)]))
    (.setTooltiptext tooltip)
    (add-event! "onClick" 
      #(apply (:gen-view scope) ((operation operations-map) (entity-type) scope)))))

(defmethod gen-option :default [option menu-type scope] 
  (gen-branch option menu-type scope))

(defn gen-ops-menu!
  "Constructs the application options menu"
  [app layout north-box scope]   
  (case (:menu-type app)
    :menubar 
    (append! north-box
      (multi-append! (Menubar.) (map 
                                  (fn [op] (gen-option op :menubar scope)) 
                                  (:ops app))))
    
    :tree
    (cascade-append! 
      [(multi-append! (Treechildren.) (map #(gen-option % :tree scope) (:ops app)))
       (Tree.)
       (doto (West.) (.setTitle "Menú de opciones")
         (.setFlex true)
         (.setMaxsize 500) 
         (.setSize "25%") 
         (.setSplittable true)
         (.setCollapsible true))
       layout])))

(defmethod gen-ref-widget :ref-form [refe scope])

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
                  (.setValue ref-box (to-str %))
                  (.setDisabled edit-button false))
        box (Hbox.)
        row (Row.)
        label (Label. (str (:single-name to-entity) ":"))]
    
    (multi-append! box
         [ref-box edit-button select-button])
    
    (add-event! select-button "onClick"
      #(gen-ref-selector to-entity setter (:ref-name refe) (.getPage box) scope))
    
    (add-event! edit-button "onClick"
      #(apply (:gen-view scope) (gen-form to-entity @reference scope)))
    
    (.setDisabled edit-button true)
       
    ;;return value
    (Reference-widget-wrapper. 
      (:widget-type refe)
      label
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

(defmethod gen-ref-widget :selector
  [refe scope]
  (let [from-entity ((:from-entity ((:rel refe))))
        record-map (ref nil)
        selector-wrapper (ref nil)]
    (Many-ref-widget-wrapper.
      :selector 
      []
      []
      (fn [] )
      (fn [r-map] (dosync (ref-set record-map r-map)))
      (fn [b] )
      (fn [b] )
      (fn [& args] (apply (second @selector-wrapper) args))
      (fn 
        ([] 
          (multi-append! (Row.)
            [(Label. (str (:ref-name refe) ":"))
             (doto 
               (first (dosync @(ref-set selector-wrapper (gen-selector from-entity 
                                                           (:mutual-ref refe) 
                                                           @record-map
                                                           scope)))) 
               (.setWidth "500px") 
               (.setHeight "500px"))]))))))

(comment (defmethod gen-ref-widget :selector-form ()))

(defn gen-selector
  
  ;;************** When search **************
  ([entity-type scope]
    (gen-selector entity-type nil nil nil scope))
  
  ;;************** When in ref **************
  ([entity-type selection scope]
    (gen-selector entity-type selection nil nil scope))
  
  ;;************** When list selector **************
  ([entity-type reference record scope] 
    (gen-selector entity-type nil reference record scope))
  
  ([entity-type selection reference ref-map scope]
    
    (let [{:keys [search-order]} entity-type
          in-ref?           (not (nil? selection))
          list-selector?    (not (nil? ref-map))
          table-state-order [:page :per-page :sort-field :sort-order]
          table-state       (ref {:page 0 :per-page 30 :sort-field nil :sort-order nil})
          criteria-order    [:example :key-search]
          criteria          (ref {:example    (record-map entity-type) 
                                  :key-search nil})
          paging            (ref nil)
          update-button     (ref nil) 
          table             (ref nil)
          search-panel      (ref nil)
          record-maps       (ref [])
          widgets           (make-widgets search-order scope reference ref-map)]
      
      (letfn 
        [(make-state []
           (dosync 
             (doseq [[widget value] 
                     {paging (gen-paging)
                      update-button (gen-update-button)
                      table (gen-table)
                      search-panel (gen-adv-search)}]
               (ref-set widget value))))
        
         (on-load-page
           [])
        
         (load-page
           []
           (let [result (apply search-with-criteria 
                          (concat
                            (filter #(not (nil? %)) 
                              (map #(% @criteria) criteria-order))
                            (do 
                              (dosync (alter table-state assoc :page (.getActivePage @paging)))
                              (map #(% @table-state) table-state-order))))
                 rows (second result)]
             (dosync (ref-set record-maps rows))
             (doto (.getModel @table) 
               (.clear)
               (.addAll (reduce
                          #(doto  %1 (.add %2))
                          (ArrayList.)
                            rows)))
              (.setTotalSize @paging (first result))
              (when in-ref?
                (selection nil))
              (.setDisabled @update-button true)))
        
          (key-search
            [key-string]
            (dosync (alter criteria
                      assoc
                      :key-search 
                      (if key-string
                        (str "%" key-string "%")
                        nil)))
            ;(println key-string)
            (load-page))
          
          ;;  ********** NORTH ***********
          
          (gen-toolbar
            []
            (defn- gen-search-box
              []
              (doto (Bandbox.)
                (.addEventListener "onChanging"
                  (proxy [EventListener][]
                    (onEvent [e]
                      (println (.getValue e))
                      (key-search (.getValue e)))))
                (#(add-event! % "onOK"
                    (fn []
                      (key-search (.getValue %)))))))
            (defn- gen-search-toggle
              [search-box]
              (doto (Toolbarbutton. "Búsqueda avanzada")
                (.setImage (:down icons))
                (#(add-event! % "onClick" 
                    (fn [] 
                      (do
                        (if (.isOpen @search-panel)
                          (doto % 
                            (.setLabel "Búsqueda por texto")
                            (.setImage (:up icons)))
                          (doto % 
                            (.setLabel "Búsqueda avanzada")
                            (.setImage (:down icons))
                            (fn [] (dosync (alter criteria assoc :key-search nil)))))
                        (.setDisabled search-box (not (.isOpen @search-panel)))
                        (.setOpen @search-panel (not (.isOpen @search-panel)))))))))
            
            (multi-append!
              (doto (Toolbar.) (.setHflex "1"))
              (let [search-box (gen-search-box)]
                (cons
                  search-box 
                  (if @search-panel
                    [(gen-search-toggle search-box)]
                    nil)))))
          
          (gen-create-button
            []
            (doto (Menuitem. "Agregar") (.setImage (:add icons))
              (add-event! "onClick"
                (if list-selector? 
                  #(apply (:gen-view scope) (gen-form entity-type reference ref-map scope))
                  #(apply (:gen-view scope) (gen-form entity-type scope))))))
          (gen-update-button
            []
            (doto (Menuitem. "Editar") (.setDisabled true) (.setImage (:edit icons))
              (add-event! "onClick"
                #(apply (:gen-view scope) 
                   (gen-form entity-type 
                     (nth @record-maps (.getSelectedIndex @table)) 
                     scope)))))
          (gen-delete-button
            []
            (doto (Menuitem. "Eliminar") (.setDisabled true) (.setImage (:remove icons))))
          
          (gen-menu
            []
            (multi-append! (Menubar.)
              [(gen-create-button) @update-button (gen-delete-button)]))
          
          (gen-north
            []
            (cascade-append! 
              [(multi-append! (doto (Vlayout.) (.setHflex "1"))
                 (if-not in-ref?
                   [(gen-toolbar) (gen-menu)]
                   [(gen-toolbar)])) 
               (doto (North.) (.setFlex true))]))
          
          ;;  ********** CENTER ***********
          
          (gen-adv-search 
            []
              (letfn 
                [(make-search 
                   []
                   (let [example (make-example entity-type widgets [:optional :interval])]
                     (dosync 
                       (alter criteria assoc :from-example (first example))
                       (alter criteria assoc :to-example (second example)))
                     (load-page)))
                      
                 (gen-search-button
                   []
                   (doto (Menuitem. "Buscar")
                     (.setImage (:find icons))
                     (add-event! "onClick" make-search)))
                 
                 (gen-clear-search-button
                   []
                   (doto (Menuitem. "Reiniciar búsqueda")
                     (.setImage (:remove icons))
                     (add-event! "onClick"
                       #(do 
                          (restart-widgets 
                            (filter
                              (fn [att-ref wrapper]
                                (not (and list-selector? (= att-ref reference)))) 
                              widgets))
                          (make-search)))))
                 
                 (gen-search-menu
                   []
                   (multi-append! (Menubar.) 
                     [(gen-search-button) (gen-clear-search-button)]))]
                
                (multi-append! (doto (Groupbox.) (.setMold "3d") (.setOpen false))
                  [(gen-search-menu)  
                   (gen-grid (make-rows (map widgets (:search-order entity-type))) "15%")])))
          
          (gen-table
            []
            (defn- gen-headers
              []
              (reduce merge
                (for [att-ref (:selector-order entity-type)]
                  {att-ref
                   (doto (Listheader. (if (is-type? att-ref Attribute)
                                        (:att-name att-ref)
                                        (:ref-name att-ref)))
                     (.setSort "auto"))})))
            
            (defn- add-header-event!
              [[att-ref header] headers]
              (doto header
                (.addEventListener "onSort"
                  (proxy [EventListener] []
                    (onEvent [e]
                      (doseq [h headers]
                        (when (not (= h header))
                          (.setSortDirection h "natural")))
                      (let [opp {"ascending" "descending", "natural" "ascending", "descending" "ascending"}
                            sort-dir {"ascending" "#asc", "descending" "#desc"}
                            dir (.getSortDirection header)]
                        (.setSortDirection header (opp dir))
                        (dosync (alter table-state merge {:sort-order (sort-dir (opp dir))
                                                          :sort-field att-ref})))
                      (.stopPropagation e)
                    (load-page))))))
            
            (defn- gen-render
              [table]
              (.setItemRenderer table 
                (proxy [ListitemRenderer] []
                  (render [item record-map]
                    (doseq [att-ref (:selector-order entity-type)]
                      (.setParent (Listcell.
                                    (let [value (get (children record-map) att-ref)]
                                      (if (nil? value)
                                      ""
                                      (to-str value))))
                        item))))))
            
            (defn- gen-table-head
              []
              (let [headers (gen-headers)]
                (multi-append! (Listhead.) 
                  (for [header headers]
                    (add-header-event! header (vals headers))))))
            
            (defn- gen-table-widget
              []
              (doto (Listbox.) 
                (.setModel (ListModelList.)) 
                (.setRows 30) 
                gen-render
                (#(add-event! % "onSelect"
                  (fn []
                    (do
                      (.setDisabled @update-button false)
                      (when in-ref?
                        (selection (nth @record-maps (.getSelectedIndex %))))))))))
            
             (cascade-append!
               [(gen-table-head) 
                (gen-table-widget)]))
          
          (gen-center
            []
            (cascade-append!
              [(multi-append! (Vlayout.) 
                 [@search-panel
                  (cascade-append!
                    [@table
                     (Panelchildren.)
                     (Panel.)])])
               (doto (Center.) (.setFlex true) (.setAutoscroll true))]))
          
          ;;  ********** SOUTH ***********
          
          (gen-paging
            []
            (doto (Paging.) 
              (.setPageSize (:per-page @table-state))
              (add-event!
                "onPaging"
                load-page)))
          
          (gen-south
            []
            (multi-append! (South.) [@paging]))
          
          ;;  ********** LAYOUT ***********
            
          (gen-layout
            []
              (multi-append! 
                (doto (Borderlayout.) (.setHeight "100%"))
                [(gen-south)
                 (gen-north) 
                 (gen-center)]))
          
          (signal
            [tn n o]
            (when (= tn (:table-name entity-type))
              (load-page)))
          
          (on-load
            []
            (defn- gen-wea
              []
              (cascade-append! 
                [(doto (Menuitem. "Seleccionar preexistente(s)")
                   (add-event! "onClick"
                     #(gen-ref-selector 
                        entity-type  
                        (fn[x](do
                                ;; ACA HAY UN TEMA !!!!!!!!!!!!!!!!!!!!!!!! 
                                ;; QUE PASA PARA VARIOS OBJETOS SELECCIONADOS
    ;                            (update entity-type x 
     ;                             (merge x (to-fks-ref
      ;                                       (:fks-pks ((:rel (val reference))))
       ;                                      ref-record)))
                                (load-page))) 
                        (str (:ref-name (val reference)) 
                          " para " 
                          (:single-name ((:to-entity (val reference)))))
                        ;(.getPage tab)
                        scope 
                        nil)))]))
              (when list-selector?
                (when (not (:not-null? ((:rel (val reference))))) 
                  (gen-wea)))
              (make-state)
              
              (load-page))]
        
        (on-load)
        
        ;;  ********** RETURN ***********
        
        (if (or in-ref? list-selector?)
          [(gen-layout) signal]
          [(gen-tab (str "Búsqueda " (:multi-name entity-type)) (:icon entity-type))  
           (gen-tab-panel (gen-layout))
           signal])))))
  
(defn gen-form
  
  ;;************** On create **************
  ([entity-type scope]
    (gen-form entity-type {entity-type {}} nil nil scope))
  
  ;;************** On edit **************
  ([entity-type record-map scope]
    (gen-form entity-type record-map nil nil scope))
  
  ;;************** On create from reference **************
  ([entity-type from-ref ref-map scope]
    (gen-form entity-type {entity-type {}} from-ref ref-map scope))
  
  ([entity-type from-map from-ref ref-map scope]
    
  (let [referring? (not (nil? from-ref))
          editing? (ref (not (empty? (children from-map))))
          form-widgets (make-widgets (:form-order entity-type) scope from-ref ref-map)
          [many-ref-widgets widgets] (map #(reduce merge {} %) (split-filter #(is-type? (key %) Many-ref) form-widgets))
          record-map (ref from-map)
          tab-panel (ref nil)
          tab (ref nil)
          save-button (ref nil)
          grid (ref nil)]
      
      (letfn 
        [(make-state
           []
           (dosync
             (ref-set grid (gen-grid (make-rows 
                                       (map (if @editing? form-widgets widgets) 
                                         (:form-order entity-type)))
                             "15%")) 
             (doseq [[widget value] (array-map
                                      tab-panel (gen-tab-panel (gen-form-layout 
                                                                 (gen-menu) 
                                                                 @grid))
                                      tab (gen-form-tab)
                                      save-button (gen-save-button))]
               (ref-set widget value))))

         (disable-pks
           []
           (doseq [pk (keys (select-pks @record-map))]
             ((:enabler (get widgets pk)) false)))

         (set-tab-label!
           ([]
             (set-tab-label! @tab))
           ([tab]
             (doto tab
               (.setLabel 
                 (if @editing? 
                   (str "Editando " (:single-name entity-type) " | " ((:to-str entity-type) @record-map)) 
                   (str "Creando " (:single-name entity-type)))))))

         (save-form
           []
           (defn- throw-result-msg
             [result]
             (if (number? result)
               (do 
                 ()          ;; TODO: MENSAJES DE ERROR
                 false)
               result))
           (defn- query 
             [fun params]
             (let [result (throw-result-msg (apply fun params))]
               (when result
                 (let [new-map (if (and (not @editing?) (:auto-inc-pk? entity-type))
                                 (hack-map->record-map (last @result))
                                 @record-map)]
                   ((scope :signal) (:table-name entity-type) @record-map new-map)
                   (dosync (ref-set record-map new-map))
                   (set-tab-label!)
                   (when-not @editing?
                     (make-many-ref-rows)
                     (.setLabel @save-button "Guardar cambios")
                     ;(disable-pks)
                     )
                   (dosync (ref-set editing? true))))))
           (let [new-map (assoc @record-map entity-type 
                           (merge (children @record-map) (children (make-example entity-type widgets))))]
             (if @editing?
               (query update [@record-map new-map ])
               (query create [new-map]))))
                
         (make-ref-signals
           []
           (map :signal 
             (filter 
               #(is-type? %1 ::ref-type)
               widgets)))
         
         (make-many-ref-rows
           []
           (let [rows (.getRows @grid)]
             (doseq [{:keys [setter gen-widget]} many-ref-widgets]
               (setter @record-map)
               (multi-append! rows (gen-widget)))))
          
         (gen-save-button
           []
           (doto (Menuitem. 
                   (if @editing? 
                     "Guardar cambios" 
                     (str "Crear " (:single-name entity-type))))
             (.setImage (:save icons))
             (add-event!  "onClick"            
               save-form)))
         
         (gen-menu
           []
           (append! (Menubar.) 
             (gen-save-button)))
          
         (gen-form-tab
           []
           (defn- add-close-event!
             [tab]
             
             (defn- on-close
               []
               (.setSelected tab false)
               ((scope :close-signal) tab)
               (.detach tab)
               (.detach @tab-panel))
             
             (defn- close-event
               [e]
               (when @editing? 
                 (if (distinct? (select-keys 
                                  (children (make-example entity-type widgets)) 
                                  (keys (children @record-map)))
                       (children @record-map))
                   (do
                     (.stopPropagation e)
                     (Messagebox/show  
                       "¿Guardar los cambios del formulario antes de cerrar?"
                       (reduce bit-xor [Messagebox/NO Messagebox/CANCEL Messagebox/YES])                                  
                       Messagebox/QUESTION
                       (proxy [EventListener] []
                         (onEvent[e]
                           (case (.. e getData intValue)
                             Messagebox/YES
                             (do
                               (save-form)
                               (on-close))
                             Messagebox/NO
                             (on-close)
                             Messagebox/CANCEL
                             (.setSelected tab true))))))
                       ((scope :close-signal) tab))))
             
             (.addEventListener tab "onClose"
               (proxy [EventListener][]
                 (onEvent[e]
                   (close-event e)))))
           
           (doto (gen-tab (:icon entity-type)) 
             set-tab-label!
             add-close-event!))
         
         (signal 
           [table-name new-rec old-rec]
           (doseq [s (make-ref-signals)]
             (s table-name new-rec old-rec))
           (when (= old-rec @record-map)
             (dosync (ref-set record-map new-rec))
             (set-widgets-values widgets @record-map)))
         
         (on-load
           []  
           (make-state)
           (when @editing?
             (set-widgets-values widgets @record-map)
             (make-many-ref-rows)
             ;(disable-pks)
             ))]
        
        (on-load)
        
      ;;Return
      [@tab
       @tab-panel
       signal
       @record-map
       #(and @editing? (is? @record-map %))]))))

(defn gen-bulk-create
  
  ([entity-type scope]
     (gen-bulk-create entity-type nil nil scope))
  
  ([entity-type reference ref-map scope]
    (letfn 
      [(throw-result-msg
         [result]
         (if (number? result)
           ()
           ()))
       
       (bulk-create
         ([example quantity]
           (create (repeat quantity example)))
         ([example begin end att]
           (create (map #({entity-type (assoc example att %)}) (range begin end)))))   
       
       (gen-create-button
         [widgets range-wrapper]
         (doto (Menuitem. (str "Crear " (:multi-name entity-type)))
           (.setImage (:save icons))
           (add-event! "onClick" 
             #(throw-result-msg
                (apply bulk-create 
                  (cons
                    (make-example entity-type widgets)
                    (let [values ((:getter range-wrapper))]
                      (if (coll? values)
                        (conj values
                          (:enum-att entity-type))
                        values))))))))
       
       (gen-layout 
         []
         (let [enum-att (:enum-att entity-type)
               range-wrapper (if enum-att
                               (gen-interval-widget enum-att)
                               (gen-quantity-widget entity-type))
               widgets (make-widgets (:bulk-create-order entity-type) scope)]
           (gen-form-layout 
             (append! (Menubar.)
               (gen-create-button widgets range-wrapper))
             (gen-grid (make-rows (cons range-wrapper (map widgets (:bulk-create-order entity-type)))) "15%"))))]
      
        [(gen-tab (str "Creación múltiple " (:multi-name entity-type)) (:icon entity-type))
         (gen-tab-panel (gen-layout))
         (fn [a b c] ())])))

(defn gen-ref-selector
  [entity-type setter ref-name page scope]
  (let [reference (ref {entity-type {}})]
    (letfn
      [(gen-select-button
         [win]
         (doto (Menuitem. "Finalizar selección") (.setDisabled true) (.setImage (:accept icons))
           (add-event! "onClick"
             #(do
                (setter @reference)
                (.detach win)))))
       
       (gen-menu
         [select-button]
         (multi-append! (Menubar.)
           [select-button]))
       
       (gen-window
         [] 
         (doto (Window. (str "Selección " ref-name) "normal" true) (.setWidth "600px") (.setHeight "400px")
           #(when (:icon entity-type)
              (multi-append! % 
                [(Caption. (str "Selección " ref-name) ((:icon entity-type) icons))]))
           (.setPage page)
           (.setMode "modal")))
       
       (make-selection-setter
         [select-button]
         #(do
            (.setDisabled select-button (nil? %))
            (dosync (ref-set reference %))))
       
       (gen-inner-selector
         [selection-setter]
         (first 
           (gen-selector 
             entity-type 
             selection-setter
             scope)))]
      
      (let [win (gen-window)
            select-button (gen-select-button win)]  
            (multi-append! win
              [(gen-form-layout (gen-menu select-button) (gen-inner-selector (make-selection-setter select-button)))])))))

(def operations-map 
  {:search      gen-selector
   :create      gen-form
   :bulk-create gen-bulk-create
   :create-from gen-form})