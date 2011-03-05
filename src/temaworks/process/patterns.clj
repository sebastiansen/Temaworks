(ns temaworks.process.patterns
  (:import 
    (org.zkoss.zul 
      Grid Row Rows Columns Column
      Borderlayout North Center))
  (:use temaworks.handling.aspect))

(defn gen-grid
  [rows first-column-width]
  (let [max-witdth-column (fn [](apply max (map #(.getWidth (first (.getChildren %))) rows)))]
    (cascade-append!
      [(multi-append! (Rows.) rows)
       (doto (Grid.) (.setHflex "1")
         (.setSpan true)
         (.setStyle "border: 0px;")
         (.appendChild (doto (Columns.) (.appendChild (doto (Column.) (.setWidth first-column-width))))))])))

(defn gen-form-layout
  [menu grid]
  (multi-append! (Borderlayout.)
    [(doto (North.) (.appendChild menu))
     (doto (Center.) (.setFlex true) (.appendChild grid))]))

