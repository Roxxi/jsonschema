(ns jsonschema.parser-test
  (:use clojure.test
        clojure.pprint
        jsonschema.parser))

(deftest test-nested-escaped-structure []
  (let [nested-json-string (slurp "test/jsonschema/nested-escaped-json.js")]
    (is (= (parse-json-string nested-json-string)
           {"level1" {"level2_0" [{"level3_i0_p0_key" 1, "level3_i1_p1_key" 10}
                                  {"level3_i1_p0_key" 2, "level3_i1_p1_key" 20}
                                  {"level3_i2_p0_key" 3, "level3_i2_p1_key" 1}],
                      "level2_1" 18824,
                      "level2_2" 906463}}))))

(deftest test-big-integer []
  (is (= (parse-json-string "{\"a\" : \"123456789012345\"}")
         {"a" 123456789012345})))


