(ns jsonschema.core
  (:use clojure.java.io
        jsonschema.parser)
  (:require [jsonschema.type-system.merge :as m]
            [jsonschema.type-system.simplify :as s]
            [jsonschema.type-system.extract :refer [extract-type-merging]]))

(def schema-analysis-mapping
  {:comprehensive m/merge-types
   :simple s/simplify-types})

(defn file-to-schema [filepath schema-merge-fn
                      & {:keys [line-xform]
                         :or {line-xform identity}}]
  (with-open [rdr (reader filepath :encoding "UTF-8")]
    (let [lines (line-seq rdr)]
      (reduce schema-merge-fn
              (map extract-type-merging
                   (parse-json-strings lines
                                       :string-transform line-xform))))))

(defn files-to-schema [filepaths schema-merge-fn
                       & {:keys [line-xform]
                          :or {line-xform identity}}]
  (reduce schema-merge-fn
          (map #(file-to-schema % schema-merge-fn :line-xform line-xform)
               filepaths)))

(defn analyze-schema [filepaths & {:keys [line-xform analysis summary]
                                   :or {line-xform identity
                                        analysis :comprehensive
                                        summary true}} ]
  (let [schema-merge-fn (schema-analysis-mapping analysis)
        schema (files-to-schema filepaths schema-merge-fn :line-xform line-xform)]
    (when summary
      (println
       (str "Analyzed " @line-number " entries. " (count @failed-lines) " failed.")))
    (clean-up!)
    schema))
