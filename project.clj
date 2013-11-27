(defproject meridian/georeference "0.0.2-SNAPSHOT"
  :description "Meridian Georeference is a library for defining and interacting with geospatial coordinate reference systems (CRS) in Clojure."
  :url "http://github.com/jsofra/georeference"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [instaparse "1.1.0"]
                 [org.geotools/gt-referencing "10.1"]
                 [org.geotools/gt-epsg-hsql "10.1"]
                 [meridian/shapes "0.0.2-SNAPSHOT"]]
  :repositories {"osgeo.org" "http://download.osgeo.org/webdav/geotools/"})
