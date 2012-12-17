(ns jsonschema.data-converters-test
  (:use clojure.test
        clojure.pprint
        clojure.template ;; for do-template
        jsonschema.data-converters
        jsonschema.type-system.types))

(defmacro type-test [type-kw & val-res-pairs]
  `(deftest ~(symbol (str (name type-kw) "-test")) []
     (testing ~(str (name type-kw) " converter")
       (let [converter# (~type-kw type-desc=>converter)]
         (do-template [val res] (is (= (converter# val) res))
                      ~@val-res-pairs)))))
(type-test :null
           nil nil
           true nil
           false nil
           5 nil
           5.5 nil
           "hello!" nil)

(type-test :bool
           nil nil
           true true
           false false
           5 true
           5.5 true
           "hello!" true)

(type-test :int
           nil nil
           true 1
           false 0
           5 5
           5.5 5
           "hello!" nil)

(type-test :real
           nil nil
           true 1.0
           false 0.0
           5 5.0
           5.5 5.5
           "hello!" nil)

(type-test :string
           nil nil
           true "true"
           false "false"
           5 "5"
           5.5 "5.5"
           "hello!" "hello!")











           
