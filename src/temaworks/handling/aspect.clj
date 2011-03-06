(ns temaworks.handling.aspect
  (:import (org.zkoss.zul
            Tabbox Tabs Tab Tabpanel Tabpanels))
  (:import (org.zkoss.zk.ui 
             Executions Desktop))
  (:import (org.zkoss.zk.ui.event
             EventListener)))

(defn add-event!
  [widget event-type event]
  (.addEventListener widget 
     event-type
     (proxy [EventListener] []
       (onEvent [e]
         (event)))))

(defn cascade-append!
  ([hierarchy]
    (reduce 
      (fn [child parent] 
        (doto parent (.appendChild child)))
        hierarchy))
  ([hierarchy & hierarchies]
    (doseq [components (cons hierarchy hierarchies)]
      (reduce (fn [child parent]
                (.appendChild parent child)
                parent)
        components))))

(defn multi-append! 
  [parent-widget children-widgets]
  (doseq [c children-widgets]
      (.appendChild parent-widget c))
  parent-widget)

(defn with-desktop*
  "Receives a procedure which is executed within a Server Push context"
  [desktop func]
  (try 
    (Executions/activate desktop) 
    (func) 
    (finally (Executions/deactivate desktop))))

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

(defn third [coll]
  (nth coll 3))

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

(defmacro with-ceremony
  "A macro that encloses the given clauses with the same bindings separatedly evaluated"
  [ceremony & clauses]
  (let [wrapped-clauses (for [clause clauses]
				  `(let [~@ceremony]
				     ~clause))]
    `(do ~@wrapped-clauses)))

(defn filter-keys
  [dict predicate]
  (select-keys dict (filter predicate (keys dict))))

(defn split-filter
  [pred coll]
  [(filter pred coll)
   (filter (comp not pred) coll)])

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) (println "\tType = " (type x#)) x#))

(defn to-class
  [s & args]
  (clojure.lang.Reflector/invokeConstructor
   (resolve (symbol s))
   (to-array args)))