(ns jsonschema.type-system.simplify
  (:use clojure.set
        roxxi.utils.print
        jsonschema.type-system.types
        jsonschema.type-system.merge))

(declare simplifying-type-merger)

;; `merge-with` will merge corresponding key-value pairs across maps
;; applying the function provided as the first arguement to `merge-with`
;; to the values _v1, v2, v3, ..._ corresponding  to key _k_ in maps
;; _m1, m2, m3, ..._, respectively.
;;
(defn compact-merge-document-document [d1 d2]
  (make-document
   (merge-with
    #(type-merge (simplifying-type-merger) % %2)
    (:map d1) (:map d2))))


(defn compact-merge-document-union [d u]
  (let [docs (document-types u)]
    (cond
      (empty? docs) (make-union (conj (:union-of u) d))
      (some #(= d %) docs) u, ;; any matches? keep the union      
      :else ;; should only ever have one doc anyway...
      (let [merged-doc (reduce compact-merge-document-document d docs)
            non-docs (non-document-types u)]
        (make-union (conj non-docs merged-doc))))))

(defn compact-merge-union-document [u d]
  (compact-merge-document-union d u))

(defn compact-merge-union-union [u1 u2]
  (let [[u1-docs u2-docs] [(document-types u1) (document-types u2)]]
    (if (and (empty? u1-docs) (empty? u2-docs))
      (make-union (union (non-document-types u1) (non-document-types u2)))
      (make-union
       (conj
        (union (non-document-types u1) (non-document-types u2))
        (reduce compact-merge-document-document
                (union (document-types u1) (document-types u2))))))))

;; see merge.clj
(def simple-type*type=>merge-fn
  (reduce extend-merge-fn-mappings
          type*type=>merge-fn
          [[:document :document compact-merge-document-document]
           [:document :union compact-merge-document-union]
           [:union :document compact-merge-union-document]
           [:union :union compact-merge-union-union]]))
  
            
(defn simplifying-type-merger []
  (make-type-merger simple-type*type=>merge-fn))

;; Convenience function
(defn simplify-types [& types]
  (let [merger (simplifying-type-merger)]    
    (reduce #(type-merge merger % %2) types)))