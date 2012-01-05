(ns georeference.wkt
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(defn WKT->clj-str [WKT]
  (letfn [(keywordize [match] (str "[:" (apply str (butlast match)) " "))]
   (str (str/replace WKT #"[A-Z][A-Z0-9_]*\[" keywordize) "]")))

(defn WKT->clj-vec [WKT]
  (read-string (WKT->clj-str WKT)))

(defmulti vec-node-transform first)
(defmethod vec-node-transform :SPHEROID
  [[key name semi-major inverse-flattening authority]]
  {key (merge {:NAME name :SEMI-MAJOR semi-major
               :INVERSE-FLATTENING inverse-flattening} authority)})
(defmethod vec-node-transform :AUTHORITY
[[key name code]] {key (str name ":" code)})
(defmethod vec-node-transform :PARAMETER
  [[key name value]] {key {(keyword name) value}})
(defmethod vec-node-transform :UNIT
  [[key name factor]] {key [name factor]})
(defmethod vec-node-transform :AXIS
  [[key name axis]] {key {:NAME name :AXIS (keyword axis)}})
(defmethod vec-node-transform :PROJECTION
  [[key name authority]] {key (merge authority {:NAME name})})
(defmethod vec-node-transform :PRIMEM
  [[key name longitude authority]]
  {key (merge {:NAME name :LONGITUDE longitude} authority)})
(defmethod vec-node-transform :TOWGS84
  [[key & params]] {key (into [] params)})
(defmethod vec-node-transform :default
  [[key name & r]]
  (let [build-map-coll
        (fn [maps k coll-k combine-fn]
          (let [items (for [x maps :when (k x)] (k x))]
            (if (empty? items) {} {coll-k (combine-fn items)})))
        axes-map (build-map-coll r :AXIS :AXES #(into [] %))
        param-map (build-map-coll r :PARAMETER :PARAMS #(apply merge %))
        remaining (->> r (remove :AXIS) (remove :PARAMETER))]
    {key (merge {:NAME name} axes-map param-map (apply merge remaining))}))

(defn clj-vec->clj-map [clj-vec-wkt]
  (walk/postwalk
   (fn [x] (if (vector? x) (vec-node-transform x) x)) clj-vec-wkt))

(defn WKT->clj-map [WKT]
  (-> WKT WKT->clj-vec WKT-clj-vec->clj-map))

(defn- str-vec-args [args]
  (str "[" (apply str (interpose ", " args)) "]"))

(defn- str-quote [s] (str "\"" s "\""))

(defmulti map-node-trans (fn [k m]  k))

(defn map-node-trans-props [k m & ks]
 (let [props (map #(map-node-trans % (k m)) ks)]
    (str (name k) (str-vec-args props))))

(defmethod map-node-trans :AUTHORITY [k m]
  (str (name k) (str-vec-args (map str-quote (str/split (k m) #":")))))
(defmethod map-node-trans :PARAMS [k m]
  (let [param-strs (for [[key val] (seq (k m))]
                     (str "PARAMETER" [(name key) val]))] 
    (apply str (interpose ", \n" param-strs))))
(defmethod map-node-trans :UNIT [k m]
  (let [[n f] (k m)] (str (name k) (str-vec-args [(str-quote n) f]))))
(defmethod map-node-trans :NAME [k m] (str-quote (k m)))
(defmethod map-node-trans :AXES [k m]
  (apply str
         (for [{:keys [NAME AXIS]} (k m)]
           (str "AXIS" (str-vec-args [(str-quote NAME) (symbol (name AXIS))]) ", \n"))))
(defmethod map-node-trans :TOWGS84 [k m] (str (name k) (str-vec-args (k m))))
(defmethod map-node-trans :PROJECTION [k m]
  (map-node-trans-props k m :NAME :AUTHORITY))
(defmethod map-node-trans :SPHEROID [k m]
  (map-node-trans-props k m :NAME :SEMI-MAJOR :INVERSE-FLATTENING :AUTHORITY))
(defmethod map-node-trans :DATUM [k m]
  (map-node-trans-props k m :NAME :SPHEROID :TOWGS84 :AUTHORITY))
(defmethod map-node-trans :PRIMEM [k m]
  (map-node-trans-props k m :NAME :LONGITUDE :AUTHORITY))
(defmethod map-node-trans :GEOGCS [k m]
  (map-node-trans-props k m :NAME :DATUM :PRIMEM :UNIT :AUTHORITY))
(defmethod map-node-trans :PROJCS [k m]
  (map-node-trans-props k m :NAME :GEOGCS :PROJECTION :PARAMS :UNIT :AXES :AUTHORITY))
(defmethod map-node-trans :default [k m] (str (k m)))

(defn clj-map->WKT [clj-map-wkt]
  (map-node-trans :PROJCS clj-map-wkt))