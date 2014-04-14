(ns jsonschema.common-db-types-test
  (:use clojure.test)
  (:require [roxxi.utils.print :refer [print-expr]]
            [jsonschema.type-system.types :as types]
            [jsonschema.type-system.db-types.common :as common])
  )

(deftest wider-than?-int-int-eq-test
  (let [l-int (types/make-int 10)
        r-int (types/make-int 10)]
    (is (not (common/wider-than? l-int r-int)))))

(deftest wider-than?-int-int-gt-test
  (let [l-int (types/make-int 11)
        r-int (types/make-int 10)]
    (is (common/wider-than? l-int r-int))))

(deftest wider-than?-int-int-lt-test
  (let [l-int (types/make-int 10)
        r-int (types/make-int 11)]
    (is (not (common/wider-than? l-int r-int)))))

(deftest wider-than?-int-str-eq-test
  (let [l-int (types/make-int 99)
        r-str (types/make-str 2 2)]
    (is (not (common/wider-than? l-int r-str)))))

(deftest wider-than?-int-str-gt-test
  (let [l-int (types/make-int 9999)
        r-str (types/make-str 3 3)]
    (is (common/wider-than? l-int r-str))))

(deftest wider-than?-int-str-lt-test
  (let [l-int (types/make-int 9999)
        r-str (types/make-str 5 5)]
    (is (not (common/wider-than? l-int r-str)))))

(deftest wider-than?-str-int-eq-test
  (let [l-str (types/make-str 2 2)
        r-int (types/make-int 99)]
    (is (not (common/wider-than? l-str r-int)))))

(deftest wider-than?-str-int-gt-test
  (let [l-str (types/make-str 3 3)
        r-int (types/make-int 99)]
    (is (common/wider-than? l-str r-int))))

(deftest wider-than?-str-int-lt-test
  (let [l-str (types/make-str 3 3)
        r-int (types/make-int 9999)]
    (is (not (common/wider-than? l-str r-int)))))
