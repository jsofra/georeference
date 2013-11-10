(defproject meridian/georeference "0.0.2-SNAPSHOT"
  :description "georeference is a library for defining and interacting with geospatial coordinate reference systems (CRS) in Clojure."
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [instaparse "1.1.0"]
                 [org.geotools/gt-referencing "9.3"]
                 [org.geotools/gt-epsg-hsql "9.3"]]
  :repositories {"osgeo.org" "http://download.osgeo.org/webdav/geotools/"})
