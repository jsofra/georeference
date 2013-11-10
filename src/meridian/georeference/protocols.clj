(ns meridian.georeference.protocols)

(defprotocol Transformable
  (transform [this from-crs to-crs]))

(defprotocol Referenced
  (get-crs [this]))
