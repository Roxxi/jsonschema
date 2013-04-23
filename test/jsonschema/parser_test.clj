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

(deftest test-another-nested-escaped-structure []
  (let [nested-json-string (slurp "test/jsonschema/nested-escaped-json2.js")]
    (is (= (parse-json-string nested-json-string)
           {"level1" {"level2_0" 1,
                      "level2_1" 0,
                      "level2_2" [{"level2_i0_p0_key" 1, "level2_i1_p0_key" 1}]}}))))

(deftest test-big-integer []
  (is (= (parse-json-string "{\"a\" : \"123456789012345\"}")
         {"a" 123456789012345})))

(deftest test-really-huge-integer []
  (is (= (parse-json-string "{\"a\" : \"89844588526466531663538850397115\"}")
         {"a" 89844588526466531663538850397115N})))

(deftest test-really-huge-decimal []
  (is (= (parse-json-string "{\"a\" : \"89844588526466531663538850397115.8984458852646650397115\"}")
         {"a" 8.984458852646652E31})))

(deftest test-a-big-url []
  (is (= (parse-json-string "{\"a\" : \"https://www.google.com/foo/20712?utm_source=Daily&utm_medium=Email&utm_campaign=23405&utm_content=3/21/2013.2553960&utm_term=New_Car.Image.2.20712\"}")
         {"a" "https://www.google.com/foo/20712?utm_source=Daily&utm_medium=Email&utm_campaign=23405&utm_content=3/21/2013.2553960&utm_term=New_Car.Image.2.20712"})))
      


