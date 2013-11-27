(ns meridian.georeference.geotools-utils
  (:import [org.geotools.referencing CRS]
           [org.opengis.referencing.operation MathTransform]
           [org.opengis.referencing.crs CoordinateReferenceSystem]))

(defn ^CoordinateReferenceSystem parse-wkt [wkt]
  (CRS/parseWKT wkt))

(defn ^CoordinateReferenceSystem resolve-name [named-crs]
  (CRS/decode (get-in named-crs [:properties :name])))

(defmulti ^CoordinateReferenceSystem resolve-link #(get-in % [:properties :type]))
(defmethod ^CoordinateReferenceSystem resolve-link "ogcwkt" [linked-crs]
  (parse-wkt (slurp (get-in linked-crs [:properties :href]))))

(defn ^CoordinateReferenceSystem resolve-srs [{:keys [type properties] :as srs}]
  (case type
    :name (resolve-name srs)
    :link (resolve-link srs)))

(defn ^MathTransform find-transform [^CoordinateReferenceSystem from-crs
                                     ^CoordinateReferenceSystem to-crs]
  (CRS/findMathTransform from-crs to-crs))
