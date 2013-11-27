(ns meridian.georeference.protocols)

(defprotocol Transformable
  (transform [this from-crs to-crs mt-lookup]))

(defprotocol Referenced
  (resolve-crs [this crs-lookup]))

(defprotocol CRS
  (->WKT [this])
  (->SRS [this]))
