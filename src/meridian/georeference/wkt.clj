(ns meridian.georeference.wkt
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [instaparse.core :as insta]))

(defn WKT->clj-str [WKT]
  (letfn [(keywordize [match] (str "[:" (apply str (butlast match)) " "))]
   (str (str/replace WKT #"[A-Z][A-Z0-9_]*\[" keywordize) "]")))

(defn WKT->clj-vec [WKT]
  (read-string (WKT->clj-str WKT)))

(defmulti vec-node-transform first)
(defmethod vec-node-transform :SPHEROID
  [[key name semi-major inverse-flattening authority]]
  {key (merge {:NAME name :SEMI-MAJOR semi-major
               :INVERSE-FLATTENING inverse-flattening} authority)})
(defmethod vec-node-transform :AUTHORITY
[[key name code]] {key (str name ":" code)})
(defmethod vec-node-transform :PARAMETER
  [[key name value]] {key {(keyword name) value}})
(defmethod vec-node-transform :UNIT
  [[key name factor]] {key [name factor]})
(defmethod vec-node-transform :AXIS
  [[key name axis]] {key {:NAME name :AXIS (keyword axis)}})
(defmethod vec-node-transform :PROJECTION
  [[key name authority]] {key (merge authority {:NAME name})})
(defmethod vec-node-transform :PRIMEM
  [[key name longitude authority]]
  {key (merge {:NAME name :LONGITUDE longitude} authority)})
(defmethod vec-node-transform :TOWGS84
  [[key & params]] {key (into [] params)})
(defmethod vec-node-transform :default
  [[key name & r]]
  (let [build-map-coll
        (fn [maps k coll-k combine-fn]
          (let [items (for [x maps :when (k x)] (k x))]
            (if (empty? items) {} {coll-k (combine-fn items)})))
        axes-map (build-map-coll r :AXIS :AXES #(into [] %))
        param-map (build-map-coll r :PARAMETER :PARAMS #(apply merge %))
        remaining (->> r (remove :AXIS) (remove :PARAMETER))]
    {key (merge {:NAME name} axes-map param-map (apply merge remaining))}))

(defn clj-vec->clj-map [clj-vec-wkt]
  (walk/postwalk
   (fn [x] (if (vector? x) (vec-node-transform x) x)) clj-vec-wkt))

(defn WKT->clj-map [WKT]
  (-> WKT WKT->clj-vec clj-vec->clj-map))

(defn- str-vec-args [args & {:keys [sep open close] :or {sep ", " open "[" close "]"}}]
  (str open (apply str (interpose sep args)) close))

(defn- str-quote [s] (str "\"" s "\""))

(defmulti map-node-trans (fn [k m]  k))

(defn map-node-trans-props [k m & ks]
 (let [props (map #(map-node-trans % (k m)) ks)]
    (str (name k) (str-vec-args props))))

(defmethod map-node-trans :AUTHORITY [k m]
  (str (name k) (str-vec-args (map str-quote (str/split (k m) #":")) :sep ",")))
(defmethod map-node-trans :PARAMS [k m]
  (let [param-strs (for [[key val] (seq (k m))]
                     (str "PARAMETER" (str-vec-args [(str-quote (name key)) val])))]
    (apply str (interpose ", \n" param-strs))))
(defmethod map-node-trans :UNIT [k m]
  (let [[n f] (k m)] (str (name k) (str-vec-args [(str-quote n) f]))))
(defmethod map-node-trans :NAME [k m] (str-quote (k m)))
(defmethod map-node-trans :AXES [k m]
  (apply str
         (interpose ", \n"
                    (for [{:keys [NAME AXIS]} (k m)]
                      (str "AXIS" (str-vec-args [(str-quote NAME) (symbol (name AXIS))]))))))
(defmethod map-node-trans :TOWGS84 [k m] (str (name k) (str-vec-args (k m))))
(defmethod map-node-trans :PROJECTION [k m]
  (map-node-trans-props k m :NAME :AUTHORITY))
(defmethod map-node-trans :SPHEROID [k m]
  (map-node-trans-props k m :NAME :SEMI-MAJOR :INVERSE-FLATTENING :AUTHORITY))
(defmethod map-node-trans :DATUM [k m]
  (map-node-trans-props k m :NAME :SPHEROID :TOWGS84 :AUTHORITY))
(defmethod map-node-trans :PRIMEM [k m]
  (map-node-trans-props k m :NAME :LONGITUDE :AUTHORITY))
(defmethod map-node-trans :GEOGCS [k m]
  (map-node-trans-props k m :NAME :DATUM :PRIMEM :UNIT :AUTHORITY))
(defmethod map-node-trans :PROJCS [k m]
  (map-node-trans-props k m :NAME :GEOGCS :PROJECTION :PARAMS :UNIT :AXES :AUTHORITY))
(defmethod map-node-trans :default [k m] (str (k m)))

(defn clj-map->WKT [clj-map-wkt]
  (map-node-trans :PROJCS clj-map-wkt))

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
horz-cs             = geographic-cs | projected-cs
projected-cs        = <'PROJCS'> <open> name <comma> geographic-cs <comma> projection <comma>
                      parameters <comma?> linear-unit (<comma> twin-axes)? (<comma> authority)? <close>
projection          = <'PROJECTION'> <open> name (<comma> authority)? <close>
geographic-cs       = <'GEOGCS'> <open> name <comma> datum <comma> prime-meridian <comma>
                      angular-unit (<comma> twin-axes)? (<comma> authority)? <close>
datum               = <'DATUM'> <open> name <comma> spheroid (<comma> to-wgs84)? (<comma> authority)? <close>
spheroid            = <'SPHEROID'> <open> name <comma> semi-major-axis <comma>
                      inverse-flattening (<comma> authority)? <close>
semi-major-axis     = number
inverse-flattening  = number
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

(def crs-transforms {:authority ->Authority
                     :axis ->Axis
                     :unit ->Unit})

(defn parse-crs [s]
  (insta/transform
   (merge crs-transforms mt-transforms common-tranforms)
   (crs-parser s)))
