(ns georeference.core
  (:import [org.geotools.referencing CRS]))

(defn get-crs [crs & {:keys [lon-first] :or {lon-first true}}]
  (CRS/decode (name crs) lon-first))

(defn calc-transform [from to]
  (CRS/findMathTransform (get-crs from) (get-crs to)))
