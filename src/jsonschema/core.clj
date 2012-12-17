(ns jsonschema.core
  (:use clojure.java.io
        jsonschema.parser
        jsonschema.type-system.extract
        jsonschema.type-system.merge))


(defn file-to-schema [filepath]
  (with-open [rdr (reader filepath :encoding "UTF-8")]
    (let [lines (line-seq rdr)
          ten-times (only-n-times 10)]
      (reduce merge-types
              (map extract-type
                        (parse-json-strings lines))))))

(defn files-to-schema [filepaths]
  (reduce merge-types
          (map file-to-schema filepaths)))

(defn analyze-schema [& filepaths]
  (let [schema (files-to-schema filepaths)]
    (clojure.pprint/pprint schema)
    (println (str "Analyzed " @line-number " entries. "
                  (count @failed-lines) " failed."))
    schema))

