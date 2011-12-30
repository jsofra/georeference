(ns georeference.core
  (:import [org.geotools.referencing CRS])
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(defn get-crs [crs & {:keys [lon-first] :or {lon-first true}}]
  (CRS/decode (name crs) lon-first))

(defn calc-transform [from to]
  (CRS/findMathTransform (get-crs from) (get-crs to)))

(defn WKT->clj-str [txt]
  (letfn [(keywordize [match] (str "[:" (apply str (butlast match)) " "))]
   (str (str/replace txt #"[A-Z][A-Z0-9_]*\[" keywordize) "]")))

(defn WKT->clj-vec [txt]
  (read-string (WKT->clj-str txt)))

(defmulti vec-node-transform first)
(defmethod vec-node-transform :SPHEROID
  [[key name semi-major inverse-flattening authority]]
  {key (merge {:NAME name :SEMI-MAJOR semi-major
               :INVERSE-FLATTENING inverse-flattening} authority)})
(defmethod vec-node-transform :AUTHORITY
  [[key name code]] {key (str name ":" code)})
(defmethod vec-node-transform :PARAMETER
  [[key name value]] {(keyword name) value})
(defmethod vec-node-transform :UNIT
  [[key name factor]] {key [name factor]})
(defmethod vec-node-transform :AXIS
  [[key name value]] {key {:NAME name :VALUE (keyword value)}})
(defmethod vec-node-transform :PROJECTION
  [[key name authority]] {key (merge authority {:NAME name})})
(defmethod vec-node-transform :PRIMEM
  [[key name longitude authority]]
  {key (merge {:NAME name :LONGITUDE longitude} authority)})
(defmethod vec-node-transform :TOWGS84
  [[key & params]] {key (into [] params)})
(defmethod vec-node-transform :default
  [[key name & r]]
    (let [axes (into [] (for [x r :when (:AXIS x)] (:AXIS x)))
          axes-map (if (empty? axes) {} {:AXES axes})
          non-axis (remove :AXIS r)]
    {key (merge {:NAME name} axes-map (apply merge non-axis))}))

(defn WKT-clj-vec->clj-map [WKT]
  (walk/postwalk
   (fn [x] (if (vector? x) (vec-node-transform x) x)) WKT))

(defn WKT->clj-map [txt]
  (-> txt WKT->clj-vec WKT-clj-vec->clj-map))