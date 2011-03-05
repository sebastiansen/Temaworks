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
      ListitemRenderer))
  (:import (java.util ArrayList))
  (:import (org.zkoss.zk.ui.event EventListener Event Events InputEvent))
  (:import (org.zkoss.zk.ui WrongValueException Executions))
  (:import (org.zkoss.util.resource Labels))
  (:import [temaworks.meta.types Entity-type Attribute Reference Relationship])
  (use 
    [clojure.contrib.seq-utils :only (positions)]
    [clojure.set])
  (:use 
    [temaworks.handling aspect crud cloudmap]
    [temaworks.meta types])
  (:require temaworks.handling.cloudmap))

(load "widgets")

(declare gen-ref-selector gen-form gen-selector)

(defn gen-grid
  [rows first-column-width]
  (let [max-witdth-column (apply max (map (comp .getWidth first .getChildren) rows))]
    (cascade-append!
      [(multi-append! (Rows.) rows)
       (doto (Grid.) (.setHflex "1")
         (.setSpan true)
         (.setStyle "border: 0px;")
         (.appendChild (doto (Columns.) (.appendChild (doto (Column.) (.setWidth max-witdth-column))))))])))

(defn gen-form-layout
  [menu grid]
  (multi-append! (Borderlayout.)
    [(doto (North.) (.appendChild menu))
     (doto (Center.) (.setFlex true) (.appendChild grid))]))

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
          criteria-order    [:from-example :to-example :key-search]
          criteria          (ref {:from-example (if list-selector? 
                                                  (assoc-ref {entity-type {}} reference ref-map)  
                                                  {entity-type {}}) 
                                  :to-example {entity-type {}} 
                                  :key-search nil})
          paging            (ref nil)
          update-button     (ref nil)
          table             (ref nil)
          search-panel      (ref nil)
          cloud-maps        (ref [])
          widgets           (make-widgets search-order scope true reference)]
      
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
                              (map #(% criteria) criteria-order))
                            (do 
                              (dosync (alter table-state assoc :page (.getActivePage @paging)))
                              (map #(% table-state) table-state-order))))
                 rows (second result)]
             (dosync (ref-set @cloud-maps rows))
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
          
          (gen-north
            []
            (doto (North.) (.setFlex true)
              (.appendChild (gen-toolbar))))
          
          ;;  ********** CENTER ***********
          
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
                     (nth @cloud-maps (.getSelectedIndex @table)) 
                     scope)))))
          (gen-delete-button
            []
            (doto (Menuitem. "Eliminar") (.setDisabled true) (.setImage (:remove icons))))
          
          (gen-menu
            []
            (multi-append! (Menubar.)
              [(gen-create-button) @update-button (gen-delete-button)]))
          
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
                   (gen-grid (make-rows (map #(get widgets %) (keys (:search-order entity-type)))) "15%")])))
          
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
                  (render [item cloud-map]
                    (doseq [att-ref (:selector-order entity-type)]
                      (.setParent (Listcell.
                                    (let [value (get (children cloud-map) att-ref)]
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
                        (selection (nth @cloud-maps (.getSelectedIndex %))))))))))
            
             (cascade-append!
               [(gen-table-head) 
                (gen-table-widget)]))
          
          (gen-center
            []
            (cascade-append!
              [(multi-append! (Vlayout.) 
                 (let [center-box [@search-panel
                                   (cascade-append!
                                     [@table
                                      (Panelchildren.)
                                      (Panel.)])]]
                   (if-not in-ref?
                     (cons (gen-menu) center-box)
                     center-box)))
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
          
          (gen-tab
            []
            (doto (Tab. (str "Búsqueda " (:multi-name entity-type))) (.setClosable true)
                   (#(if-not (nil? (:icon entity-type))
                      (.setImage % ((:icon entity-type) icons))
                      %))))
          
          (gen-tab-panel
            []
            (cascade-append!
              [(gen-layout)
               (doto (Tabpanel.) (.setHeight "100%"))]))
          
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
          [(gen-tab)  
           (gen-tab-panel)
           signal])))))

(defrecord Form-state
  [tab-panel
   tab
   many-refs-signals
   gen-save-button])
  
(defn gen-form
  
  ;;************** On create **************
  ([entity-type scope]
    (gen-form entity-type {entity-type {}} nil nil scope))
  
  ;;************** On edit **************
  ([entity-type cloud-map scope]
    (gen-form entity-type cloud-map nil nil scope))
  
  ;;************** On create from reference **************
  ([entity-type from-ref ref-map scope]
    (gen-form entity-type {entity-type {}} from-ref ref-map scope))
  
  ([entity-type from-map from-ref ref-map scope]
    
    (let [referring? (not (nil? from-ref))
          editing? (ref (not (empty? (children from-map))))
          widgets (make-widgets (:form-order entity-type) scope [:constrain])
          cloud-map (ref from-map)
          tab-panel (ref nil)
          tab (ref nil)
          many-refs-signals (ref nil)
          save-button (ref nil)]
      (letfn 
        [(make-state
           []
           (dosync
             (doseq [[widget value] {tab-panel (gen-tab-panel)
                                     tab (gen-tab)
                                     many-refs-signals (make-many-refs-signals)
                                     save-button (gen-save-button)}]
               (ref-set widget value))))
      
         (disable-pks
           []
           (doseq [pk (keys (select-pks @cloud-map))]
             ((:enabler (get widgets pk)) false)))
      
         (set-tab-label!
           ([]
             (set-tab-label! @tab))
           ([tab]
             (doto tab
               (.setLabel 
                 (if @editing? 
                   (str "Editando " (:single-name entity-type) " | " ((:to-str entity-type) @cloud-map)) 
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
                                 (hack-map->cloud-map (last @result))
                                 @cloud-map)]
                   ((scope :signal) (:table-name entity-type) @cloud-map new-map)
                   (dosync (ref-set cloud-map new-map))
                   (set-tab-label!)
                   (when-not @editing?
                     (doseq [sig @many-refs-signals]
                       (sig))
                     (.setLabel @save-button "Guardar cambios")
                     ;(disable-pks)
                     )
                   (dosync (ref-set editing? true))))))
           (let [new-map (assoc @cloud-map entity-type 
                           (merge (children @cloud-map) (children (make-example entity-type widgets))))]
             (if @editing?
               (query update [@cloud-map new-map ])
               (query create [new-map]))))
         
         (make-many-refs-signals
           []
           (map (fn [reference wrapper]
                  (fn []
                    )) 
             (filter 
               (fn [[att-ref wrapper]] 
                 (and (is-type? att-ref Reference) (= (:widget-type att-ref) :selector)))
               widgets)))
                
         (make-refs-signals
           []
           (map :signal 
             (filter 
               #(is-type? %1 Reference)
               widgets)))
          
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
           (multi-append! (Menubar.) 
             [(gen-save-button)]))
          
         (gen-tab
           []
           (defn- add-close-event!
             [tab]
             (defn- on-close
               []
               (.setSelected tab false)
               ((scope :close-signal) tab)
               (.detach tab)
               (.detach @tab-panel))
             (defn- make-msg-box-listener
               []
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
                     (.setSelected tab true)))))
             (.addEventListener tab "onClose"
               (proxy [EventListener][]
                 (onEvent[e]
                   (when @editing? 
                     (if (distinct? (select-keys 
                                      (children (make-example entity-type widgets)) 
                                      (keys (children @cloud-map)))
                           (children @cloud-map))
                       (do
                         (.stopPropagation e)
                         (Messagebox/show  
                           "¿Guardar los cambios del formulario antes de cerrar?"
                           (reduce bit-xor [Messagebox/NO Messagebox/CANCEL Messagebox/YES])                                  
                           Messagebox/QUESTION
                           (make-msg-box-listener)))
                       ((scope :close-signal) tab)))))))
           (doto (set-tab-label! (Tab.))
             (.setClosable true)
             (#(when-not (nil?(:icon entity-type))
               (.setImage % ((:icon entity-type) icons))))
             add-close-event!))
         
         (gen-tab-panel
           [] 
           (multi-append! (doto (Tabpanel.) (.setHeight "100%"))
             [(gen-form-layout 
                (gen-menu) 
                (gen-grid (make-rows (map #(get widgets %) (:form-order entity-type))) "15%"))]))
         
         (signal 
           [table-name new-rec old-rec]
           (doseq [s (make-refs-signals)]
             (s table-name new-rec old-rec))
           (when (= old-rec @cloud-map)
             (dosync (ref-set cloud-map new-rec))
             (set-widgets-values widgets @cloud-map)))
         
         (on-load
           []  
           (make-state)
           (when @editing?
             (set-widgets-values widgets @cloud-map)
             (doseq [signal @many-refs-signals]
               (signal))
             ;(disable-pks)
             )
           (when referring?
             ((:setter (get widgets from-ref)) ref-map)
             ((:unlocker (get widgets from-ref)) false)))]
        
        (on-load)
        
      ;;Return
      [@tab
       @tab-panel
       signal
       @cloud-map
       #(and @editing? (is? @cloud-map %))]))))

(defn gen-bulk-create
  
  ([entity-type scope])
  
  ([entity-type reference ref-map scope]
    
    (defn- throw-result-msg
      [result]
      (if (number? result)
        ()
        ()))
    (defn- bulk-create
      ([example quantity]
        (create (repeat quantity example)))
      ([example begin end att]
        (create (map #({entity-type (assoc example att %)}) (range begin end)))))    
    
    (defn- gen-create-button
      [widgets range-wrapper]
      (doto (Menuitem. (str "Crear " (:multi-name entity-type)))
        (.setImage)
        (add-event! "onClick" 
          #(throw-result-msg
             (apply bulk-create 
               (cons
                 (make-example entity-type widgets [:optional])
                 (let [values ((:getter range-wrapper))]
                   (if (coll? values)
                     (conj values
                       (:enum-att entity-type))
                     values))))))))
    
    (let [enum-att (:enum-att entity-type)
          range-wrapper (if (:aut-inc-pk? entity-type)
                          (gen-quantity-widget entity-type)
                          (gen-interval-widget enum-att))
          widgets (make-widgets (:bulk-create-order entity-type) scope [:optional])]
      (gen-form-layout 
        (multi-append! (Menu.)
          [(gen-create-button widgets range-wrapper)]) 
        (gen-grid (make-rows (cons range-wrapper 
                               (vals 
                                 (map 
                                   #(get widgets %)
                                   (:bulk-create-order entity-type))))) "15%")))))
  
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