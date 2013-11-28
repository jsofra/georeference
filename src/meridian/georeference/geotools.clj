(ns meridian.georeference.geotools
  (:require [meridian.georeference.geotools-utils :as utils]
            [meridian.georeference.protocols :as p]
            [meridian.georeference :as r]
            [meridian.clj-jts :as jts])
  (:import [org.geotools.referencing CRS]
           [org.geotools.geometry.jts JTS]
           [org.opengis.referencing.crs CoordinateReferenceSystem]
           [com.vividsolutions.jts.geom Geometry]
           [meridian.shapes Point LineString LinearRing Polygon
            MultiPoint MultiLineString MultiPolygon
            GeometryCollection]))

(extend-protocol p/CRS
  CoordinateReferenceSystem
  (->WKT [this] (str this))
  (->SRS [this] (CRS/toSRS this)))

(extend-protocol p/Transformable
  Geometry
  (transform [this from-crs to-crs mt-lookup]
    (JTS/transform this (mt-lookup from-crs to-crs))))

(doseq [t [Point LineString LinearRing Polygon
           MultiPoint MultiLineString MultiPolygon
           GeometryCollection]]
  (extend t p/Transformable
          {:transform (fn [this from-crs to-crs mt-lookup]
                        (jts/->shape (p/transform (jts/map->jts this)
                                                     from-crs to-crs mt-lookup)))}))

(defn transform-to
  ([referenced to-srs {:keys [crs-lookup mt-lookup] :as lookups}]
     (r/transform-to referenced to-srs lookups))
  ([referenced to-srs]
     (r/transform-to referenced to-srs
                     {:crs-lookup utils/resolve-srs
                      :mt-lookup utils/find-transform})))
