(ns meridian.georeference
  (:require [meridian.georeference.protocols :as p])
  (:import [org.opengis.referencing.crs ProjectedCRS GeographicCRS GeocentricCRS]))

(defn named-crs [name]
  {:type :name
   :properties {:name name}})

(defn linked-crs [link type]
  {:type :link
   :properties {:href link :type type}})

(defn resolve-crs [referenced crs-lookup]
  (p/resolve-crs referenced crs-lookup))

(defn transform-to [referenced to-srs {:keys [crs-lookup mt-lookup]}]
  (p/transform referenced
               (resolve-crs referenced crs-lookup) (crs-lookup to-srs)
               mt-lookup))

(defn project-to [referenced to-crs {:keys [crs-lookup mt-lookup] :as lookups}]
;  {:pre [(instance? ProjectedCRS to-crs)
;         (or (instance? GeographicCRS (resolve-crs referenced))
;             (instance? GeocentricCRS (resolve-crs referenced)))]}
  (transform-to referenced to-crs lookups))

(defn unproject-to [referenced to-crs {:keys [crs-lookup mt-lookup] :as lookups}]
;  {:pre [(instance? ProjectedCRS (resolve-crs referenced))
;         (or (instance? GeographicCRS to-crs)
;             (instance? GeocentricCRS to-crs))]}
  (transform-to referenced to-crs lookups))

(defn parse-ogc-urn [urn]
  (let [[_ nid _ object-type authority version code]
        (seq (.split urn ":"))]
    {:object-type object-type
     :authority authority
     :version version
     :code code}))
