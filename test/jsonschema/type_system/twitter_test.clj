(ns jsonschema.type-system.twitter-test
  (:use clojure.test
        clojure.pprint
        jsonschema.type-system.types
        cheshire.core)
  (:require [jsonschema.type-system.extract :refer [extract-type-merging]]
            [jsonschema.type-system.merge :refer [merge-types]]))



(deftest types-4-tweets
  (testing
      "Generating types for tweets in tweets.js
       This isn't really a test. It's for documentation"
    (let [result
          (let [parsed-js (parse-string (slurp "test/jsonschema/type_system/tweets.js"))]
            (map #(extract-type-merging %) parsed-js))]
      ;;(pprint result) ;; uncomment me to generate structure for document
      (is (coll? result)))))

(deftest type-4-tweets
  (testing
      "Generate the merged type for all of the tweets
       This isn't really a test. It's for documentation"
    (let [result
          (let [parsed-js (parse-string (slurp "test/jsonschema/type_system/tweets.js"))]
            (map #(extract-type-merging %) parsed-js))]
      ;;(pprint (apply merge-types result)) ;; uncomment me to generate structure for document
      (is true))))
