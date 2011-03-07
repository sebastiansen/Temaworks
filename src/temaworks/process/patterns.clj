(ns temaworks.process.patterns
  (:import 
    (org.zkoss.zul 
      Grid Row Rows Columns Column
      Borderlayout North Center)
    [temaworks.handling.recordmap Record-map Interval-map])
  (:use [temaworks.handling aspect]))

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

(defmulti to-str class)
(defmethod to-str Long           [x] (Long/toString x)                                              )
(defmethod to-str Integer        [x] (Integer/toString x)                                           )
(defmethod to-str Double         [x] (format "%.2f" (double x))                                     )
(defmethod to-str Float          [x] (Float/toString x)                                             )
(defmethod to-str java.util.Date [x] (format "%1$td-%1$tm-%1$tY" x)                                 )
(defmethod to-str String         [x] x                                                              )
(defmethod to-str Boolean        [x] (if x "Sí" "No")                                               )
(defmethod to-str Record-map     [x] ((:to-str (:entity-type x)) x)                                 )
(defmethod to-str Interval-map   [x] (str "Desde: " (to-str (:from x)) "  Hasta: " (to-str (:to x))))
(defmethod to-str :default       [x] (String/valueOf x)                                             )