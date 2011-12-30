(defproject meridian/georeference "0.0.1-SNAPSHOT"
  :description "georeference is a library for defining and interacting with geospatial coordinate reference systems (CRS) in Clojure."
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.geotools/gt-referencing "2.7.4"]
                 [org.geotools/gt-epsg-hsql "2.7.4"]]
  :repositories {"osgeo.org" "http://download.osgeo.org/webdav/geotools/"})
