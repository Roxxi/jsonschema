(defproject com.onekingslane.danger/jsonschema "1.2.0"
  :description "Library to help you derive schemas from arbirary collections of JSON"
  :license {:name "GNU General Public License Version 3"
            :url "http://www.gnu.org/licenses/gpl-3.0.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [com.onekingslane.danger/clojure-common-utils "0.0.19"]
                 [cheshire "4.0.3"]
                 [roxxi/simple-avro "0.0.6"]
                 [org.clojure/math.numeric-tower "0.0.2"]
                 [slingshot "0.10.3"]])
