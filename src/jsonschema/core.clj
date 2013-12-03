(ns jsonschema.core
  (:use clojure.java.io)
  (:require [jsonschema.type-system.merge :as m]
            [jsonschema.type-system.simplify :as s]
            [jsonschema.type-system.extract :as e]
            [jsonschema.parser :as p]
            [clojure.string :refer [split trim]]
            [roxxi.utils.common :refer [def-]]
            [roxxi.utils.print :refer [print-expr]]))

(def- extract-fn-mapping
  {:comprehensive e/extract-type-merging
   :simple e/extract-type-simplifying})
(def- merge-fn-mapping
  {:comprehensive m/merge-types
   :simple s/simplify-types})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Generics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- data-to-schema [line-seq line-parse-fn schema-extract-fn schema-merge-fn]
  (let [schemas (map (comp schema-extract-fn line-parse-fn) line-seq)]
    (reduce schema-merge-fn schemas)))

(defn- file-to-schema [filepath line-parse-fn schema-extract-fn schema-merge-fn]
  (with-open [rdr (reader filepath :encoding "UTF-8")]
    (let [lines (line-seq rdr)]
      (data-to-schema lines line-parse-fn schema-extract-fn schema-merge-fn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Specifics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn json-file-to-schema [filepath schema-extract-fn schema-merge-fn
                           & {:keys [line-xform]
                              :or {line-xform identity}}]
  (let [line-parse-fn #(p/parse-json-string % :string-transform line-xform)]
    (file-to-schema filepath line-parse-fn schema-extract-fn schema-merge-fn)))

(defn json-files-to-schema [filepaths schema-extract-fn schema-merge-fn
                            & {:keys [line-xform]
                               :or {line-xform identity}}]
  (let [filepath-to-schema-fn #(json-file-to-schema %
                                     schema-extract-fn
                                     schema-merge-fn
                                     :line-xform line-xform)
        schemas (map filepath-to-schema-fn filepaths)]
    (reduce schema-merge-fn schemas)))




(defn analyze-json-schema [filepaths & {:keys [line-xform analysis summary]
                                   :or {line-xform identity
                                        analysis :comprehensive
                                        summary true}} ]
  (let [schema-merge-fn (merge-fn-mapping analysis)
        schema (files-to-schema filepaths schema-merge-fn :line-xform line-xform)]
    (when summary
      (println
       (str "Analyzed " @line-number " entries. " (count @failed-lines) " failed.")))
    (clean-up!)
    schema))





(defn parse-csv-val [val]
  (or (p/number-if-number val)
      (p/parsed-if-parsed val)
      val))

(defn parse-csv-line [line delimiter header]
  (let [delimiter (re-pattern delimiter)
        raw-vals (split line delimiter)
        vals (map (comp parse-csv-val trim) raw-vals)
        header (if (= header :default)
                 (map #(str "col" %) (range (count vals)))
                 header)
        kv-pairs (map #(vector % %2) header vals)]
    (into {} kv-pairs)))

(defn csv-file-to-schema [filepath schema-extract-fn schema-merge-fn
                          & {:keys [delimiter header line-xform]
                             :or {delimiter ","
                                  header :default
                                  line-xform identity}}]
  (let [line-parse-fn #(parse-csv-line % delimiter header)]
    (file-to-schema filepath line-parse-fn schema-extract-fn schema-merge-fn)))
