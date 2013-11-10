(ns meridian.georeference
  (:require [meridian.georeference.protocols :as p])
  (:import [org.geotools.referencing CRS]
           [org.opengis.referencing.crs CoordinateReferenceSystem]
           [org.opengis.referencing.cs CoordinateSystem]
           [org.opengis.referencing.operation MathTransform]
           [org.opengis.referencing.crs ProjectedCRS GeographicCRS GeocentricCRS]))

(defn ^CoordinateReferenceSystem decode-crs [crs & {:keys [lon-first] :or {lon-first true}}]
  (CRS/decode (name crs) lon-first))

(defn ^CoordinateSystem get-cs [crs]
  (.getCoordinateSystem crs))

(defn ^MathTransform calc-transform [from-crs to-crs]
  (CRS/findMathTransform from-crs to-crs))

(defn named-crs [name]
  {:type :name
   :properties {:name name}})

(defn linked-crs [link type]
  {:type :link
   :properties {:link link :type type}})

(defn get-crs [referenced]
  (if-let [crs (get-in referenced [:crs :properties :name])]
    (decode-crs crs)
    (p/get-crs referenced)))

(defn transform-to [referenced to-crs]
  (p/transform referenced (get-crs referenced) to-crs))

(defn project-to [referenced to-crs]
  {:pre [(instance? ProjectedCRS to-crs)
         (or (instance? GeographicCRS (get-crs referenced))
             (instance? GeocentricCRS (get-crs referenced)))]}
  (transform-to referenced to-crs))

(defn unproject-to [referenced to-crs]
  {:pre [(instance? ProjectedCRS (get-crs referenced))
         (or (instance? GeographicCRS to-crs)
             (instance? GeocentricCRS to-crs))]}
  (transform-to referenced to-crs))

(defn parse-ogc-urn [urn]
  (let [[_ nid _ object-type authority version code]
        (seq (.split urn ":"))]
    {:object-type object-type
     :authority authority
     :version version
     :code code}))
