(ns meridian.georeference.wkt
  (:require [clojure.string :as str]
            [instaparse.core :as insta]))

(def common-grammer-defs "
open             = '[' | '('
close            = ']' | ')'
number           = #'[-+]?[0-9]*\\.?[0-9]+'
<name>           = <'\"'> #'[^\"]*' <'\"'>
<class-name>     = name
<param-name>     = name
integer          = #'[0-9]'
comma            = #'\\s*,\\s*'
")

(def math-transform-grammer "
math-transform    = param-mt | concat-mt | inv-mt | passthrough-mt
math-transforms   = (math-transform <comma?>)+
param-mt          = <'PARAM_MT'> <open> class-name <comma?> parameters <close>
parameter         = <'PARAMETER'> <open> param-name <comma> value <close>
parameters        =  (parameter <comma?>)*
<value>           = number
concat-mt         = <'CONCAT_MT'> <open> math-transforms <close>
inv-mt            = <'INVERSE_MT'> <open> math-transform <close>
passthrough-mt    = <'PASSTHROUGH_MT'> <open> integer <comma> math-transform <close>
")

(def crs-grammer "
<coordinate-system> = horz-cs | geocentric-cs | vert-cs | compd-cs | fitted-cs | local-cs
<horz-cs>             = geographic-cs | projected-cs
projected-cs        = <'PROJCS'> <open> name <comma> geographic-cs <comma> projection <comma>
                      parameters <comma?> linear-unit (<comma> twin-axes)? (<comma> authority)? <close>
projection          = <'PROJECTION'> <open> name (<comma> authority)? <close>
geographic-cs       = <'GEOGCS'> <open> name <comma> datum <comma> prime-meridian <comma>
                      angular-unit (<comma> twin-axes)? (<comma> authority)? <close>
datum               = <'DATUM'> <open> name <comma> spheroid (<comma> to-wgs84)? (<comma> authority)? <close>
spheroid            = <'SPHEROID'> <open> name <comma> semi-major-axis <comma>
                      inverse-flattening (<comma> authority)? <close>
<semi-major-axis>   = number
<inverse-flattening>= number
prime-meridian      = <'PRIMEM'> <open> name <comma> longitude (<comma> authority)? <close>
longitude           = number
<angular-unit>      = unit
<linear-unit>       = unit
unit                = <'UNIT'> <open> name <comma> conversion-factor (<comma> authority)? <close>
conversion-factor   = number
geocentric-cs       = <'GEOCCS'> name <comma> datum <comma> prime-meridian <comma> linear-unit <comma>
                      (axis <comma> axis <comma> axis <comma?>)? authority? <close>
authority           = <'AUTHORITY'> <open> name <comma> code <close>
vert-cs             = <'VERT_CS'> <open> name <comma> vert-datum <comma> linear-unit
                      (<comma> axis)* (<comma> authority)? <close>
vert-datum          = <'VERT_DATUM'> <open> name <comma> datum-type (<comma> authority)? <close>
datum-type          = number
compd-cs            = <'COMPD_CS'> <open> name <comma> head-cs <comma> tail-cs (<comma> authority)? <close>
head-cs             = coordinate-system
tail-cs             = coordinate-system
twin-axes           = axis <comma> axis <comma?>
axis                = <'AXIS'> <open> name <comma>
                      ('NORTH' | 'SOUTH' | 'EAST' | 'WEST' | 'UP' | 'DOWN' | 'OTHER') <close>
to-wgs84            = <'TOWGS84'> <open> seven-param <close>
seven-param         = dx <comma> dy <comma> dz <comma> ex <comma> ey <comma> ez <comma> ppm
dx                  = number
dy                  = number
dz                  = number
ex                  = number
ey                  = number
ez                  = number
ppm                 = number
fitted-cs           = <'FITTED_CS'> <open> name <comma> to-base <comma> base-cs <close>
to-base             = math-transform
base-cs             = coordinate-system
local-cs            = <'LOCAL_CS'> <open> name <comma> local-datum <comma> unit
                      (<comma> axis)+ (<comma> authority)? <close>
local-datum         = <'LOCAL_DATUM'> <open> name <comma> datum-type (<comma> authority)? <close>
<code>              = name
")

(def common-tranforms {:parameter vector
                       :parameters #(into {} %&)
                       :number #(java.lang.Double/parseDouble %)})

(def math-transform-parser
  (insta/parser (str math-transform-grammer
                     common-grammer-defs)))

(defn transform-fn [req opt]
  (fn [& vals]
    (zipmap req vals)
    ))



(defrecord ParamMT [name params])
(defrecord ConcatMT [math-transforms])
(defrecord InvMT [math-transform])
(defrecord PassThroughMT [index math-transform])

(def mt-transforms {:math-transform identity
                    :math-transforms vector
                    :param-mt ->ParamMT
                    :concat-mt ->ConcatMT
                    :inv-mt ->InvMT
                    :passthrough-mt ->PassThroughMT})

(defn parse-math-transform [s]
  (insta/transform
   (merge mt-transforms common-tranforms)
   (math-transform-parser s)))

(def crs-parser (insta/parser (str crs-grammer
                                   math-transform-grammer
                                   common-grammer-defs)))

(defrecord ProjectedCS [name geographic-cs projection params unit axes authority])
(defrecord Projection [name authorities])
(defrecord GeographicCS [name datum prime-meridian unit axes authority])
(defrecord Authority [name code])
(defrecord Axis [name dimension])
(defrecord Unit [name conversion-factor authority])
(defrecord Datum [name spheroid towgs84 authority])
(defrecord Spheroid [name semi-major-axis inverse-flattening authority])

(def crs-transforms {:authority ->Authority
                     :axis ->Axis
                     :unit ->Unit
                     :spheroid ->Spheroid
                     ;:datum ->Datum
                     ;:geographic-cs ->GeographicCS
                     })

(defn parse-crs [s]
  (insta/transform
   (merge crs-transforms mt-transforms common-tranforms)
   (crs-parser s)))
