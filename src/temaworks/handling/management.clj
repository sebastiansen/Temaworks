(ns temaworks.handling.management
  (:import (org.zkoss.zul
            Window
            ;Groupbox
            ;Bandbox
            Image
            ;Listbox Listhead Listheader Listitem
            Columns Column
            Vbox
            Borderlayout Center West North South
            Grid Row Rows
            Label Textbox 
            Menu Menubar Menupopup Menuitem Messagebox
            Tabbox Tabs Tab Tabpanel Tabpanels
            Tree Treeitem Treerow Treecell Treechildren
            Toolbar Toolbarbutton
            Button
            Popup)
    (org.zkoss.zk.ui.event
      EventListener Event Events)
    (org.zkoss.zk.ui Sessions))
  (:use
    temaworks.meta.types
    temaworks.handling.prueba
    temaworks.handling.crud
    temaworks.handling.aspect
    temaworks.handling.recordmap
    temaworks.process.views
    temaworks.meta.authorization))

(defn gen-scope
  ""
  [tabs tab-panels tab-box]
  (let [signals-map (ref {})           ;; Tab → (Keyword → Record → Record) - in scope
        has-cloud-map?-map (ref {})]   ;; Tab → (Record → Bool) - in scope
    (letfn [(gen-view
              ;; Not associated with a single instance - in scope
              ([tab tab-panel signal]
                (cascade-append!
                  [tab tabs]
                  [tab-panel tab-panels])
                (.setSelectedTab tab-box tab)
                (dosync (alter signals-map assoc tab signal)))
              
              ;; Can only be opened once
              ([tab tab-panel signal cloud-map has-cloud-map?]
                (if (not (empty? @has-cloud-map?-map))
                  (loop [views (seq @has-cloud-map?-map)]
                    (if ((val (first views)) cloud-map)
                      (.setSelected (key (first views)) true)
                      (if (= (first views) (last @has-cloud-map?-map))
                              (do
                                (gen-view tab tab-panel signal)
                                (dosync (alter has-cloud-map?-map assoc tab has-cloud-map?)))
                              (recur (next views)))))
                  (do
                    (gen-view tab tab-panel signal)
                    (dosync (alter has-cloud-map?-map assoc tab has-cloud-map?))))))]
      {:signal       (fn 
                       [entity-type new-rec old-rec]
                       (doseq [[tab signal] @signals-map]
                         (signal entity-type new-rec old-rec)))
       
       :close-signal (fn 
                       [tab]
                       (dosync 
                         (alter signals-map dissoc tab)
                         (alter has-cloud-map?-map dissoc tab)))
       
       :gen-view     gen-view})))
  
(defn gen-tab-box
  []
  (let [[tabs tab-panels tab-box] [(Tabs.) (Tabpanels.) (Tabbox.)]
        scope (gen-scope tabs tab-panels tab-box)]
    
    (defn- gen-tab-bar []
      (append! (Toolbar.) 
        (doto (Toolbarbutton.)
          (add-event! "onClick"
            #(Messagebox/show  
               "Desea cerrar todas las pestañas abiertas?"
               "Cerrar todas las pestañas"
               (bit-xor Messagebox/YES Messagebox/NO)                                  
               Messagebox/QUESTION
               (proxy [EventListener] []
                 (onEvent[e]
                   (when (= (.. e getData intValue) Messagebox/YES)
                     (doseq [t (apply vector (.getChildren tabs))]
                       (.detach t)
                       ((:close-signal scope) t))
                     (doseq [tp (apply vector (.getChildren tab-panels))]
                      (.removeChild tab-panels tp)))))))
          (.setImage (:cancel icons))
          (.setTooltiptext "Cerrar todas las pestañas abiertas"))))
    
    (multi-append! tab-box 
      [(gen-tab-bar) tabs tab-panels])
    [tab-box scope]))

(defn load-gui [composer layout desktop page]
  (let [[tab-box scope] (gen-tab-box)
        north-box (doto (Vbox.) (.setHeight "100%"))]
    ;;(doto (Menuitem. "Cerrar Sesión") (.setImage (:key icons)))
    (multi-append! layout
      [(cascade-append!
         [(doto (Image. ((:image app) icons)) (.setHeight "50px"))
          north-box 
          (doto (North.) (.setFlex true))])
       (cascade-append!
         [tab-box 
          (doto (Center.) (.setFlex true))])])
    (gen-ops-menu! app layout north-box scope)))

(defn launch-sign-in [composer layout desktop page]
  (let [win (doto (Window. "Autenticación" "normal" true) (.setPage page) (.setMode "modal") (.setWidth "554px"))
        box (doto (Vbox.) (.setHeight "100%"))
        form (doto (Grid.) (.appendChild (doto (Columns.) (.appendChild (doto (Column.) (.setWidth "100px"))))))
        user (Textbox.)
        pass (doto (Textbox.) (.setType "password"))
        user-row (Row.) 
        pass-row (Row.)
        rows (Rows.)
        menu (Menubar.)
        access #(if (exists? (hack-map->record-map temaworks-user {:user (.getValue user) :pass (.getValue pass)}))
                  (do
                    (.detach win)
                    (load-gui))
                  (Messagebox/show  
                    "Nombre de usuario o contraseña incorrecto."
                    "Error"
                    Messagebox/OK
                    Messagebox/EXCLAMATION))]
    (add-event! user "onOK" access)
    (add-event! pass "onOK" access)
    (cascade-append!
      [(Label. "Usuario:") user-row rows form]
      [(Label. "Contraseña:") pass-row rows]
      [user user-row]
      [pass pass-row]
      [(doto (Menuitem."Ingresar") (add-event! "onClick" access) (.setImage (:signin icons))) menu box win])
    
    (doseq [x [(doto (Image. (:app-icon icons)) (.setWidth "554px")) form menu]] ;(.setHeight "50px")
      (.appendChild box x))
    (.setTitle page (:name app))
    ;(.setHflex form "min")
    ))

(defn init 
  "Initiates the application in the CLOUD!"
  [composer layout desktop page]
  ;(launch-sign-in composer layout desktop page)
  (load-gui composer layout desktop page))
