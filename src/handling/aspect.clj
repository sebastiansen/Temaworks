(ns handling.aspect
  (:import (org.zkoss.zul
            Tabbox Tabs Tab Tabpanel Tabpanels))
  (:import (org.zkoss.zk.ui 
             Executions Desktop))
  (:import (org.zkoss.zk.ui.event
             EventListener)))

(defn add-event
  [widget event-type event]
  (.addEventListener widget 
     event-type
     (proxy [EventListener] []
       (onEvent [e]
         (event)))))

(defn cascade-append
  [hierarchy & hierarchies]
  (doseq [components (cons hierarchy hierarchies)]
    (reduce (fn [child parent] 
              (.appendChild parent child) parent)
      components)))

(defn with-desktop*
  "Receives a procedure which is executed within a Server Push context"
  [desktop func]
  (try 
    (Executions/activate desktop) 
    (func) 
    (finally (Executions/activate desktop))))

(defmacro with-desktop
  "A macro which executes its body within a Server Push context"
  [bind & body]
  (assert (vector? bind))
  (let [desktop (if (even? (count bind))
                  (second bind)
                  (first bind))
        variable (if (even? (count bind))
                   (first bind)
                   (gensym "desktop"))]    
    `(let [~variable ~desktop]
       (with-desktop* ~variable
         (fn []
           ~@body)))))

(def icons
  {:add "/img/add.png"
   :remove "/img/delete.png"
   :edit "/img/pencil.png"
   :accept "/img/accept.png"
   :group "/img/group.png"
   :building "/img/building.png"
   :briefcase "/img/briefcase.png"
   :save "/img/disk.png"
   :policys nil
   :key "/img/key.png"
   :up "/img/arrow_up.png"
   :find "/img/find.png"
   :app-icon "/img/generalesvida.png"
   :cancel "/img/cancel.png"
   :email "/img/email.png"
   :signin "/img/door_in.png"
   :down "/img/arrow_down.png"})
