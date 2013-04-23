(ns jsonschema.parser
  (:use roxxi.utils.print
        roxxi.utils.collections
        cheshire.core))


(def line-number (atom 0))
(def failed-lines (atom []))
(defn clean-up! []
  (swap! line-number (fn [foo] 0))
  (swap! failed-lines (fn [foo] [])))

(def failed-lines (atom []))
(defn parse-string-with-notify [line]
  (swap! line-number inc)
  (try
    ;; (when (= @line-number 305868)
    ;;   (do
    ;;     (println line)
    ;;     (println parse-string line)))
    (parse-string line)
    (catch com.fasterxml.jackson.core.JsonParseException e
      (swap! failed-lines #(conj @failed-lines %))
      (println
       (str "Line #: " @line-number ", " (.getMessage e) "\n"
            line "\n---------"))
      )))
       

(defn- first-and-last-char-are [str-val first-c last-c]
  (and
   (= (get str-val 0) first-c)
   (= (get str-val (- (count str-val) 1)) last-c)))

(defn- array-ish? [str-val]
  (first-and-last-char-are str-val \[ \]))

(defn- map-ish? [str-val]
  (first-and-last-char-are str-val \{ \}))

(defn- unescape-one-level [string]
  ;; Previous
  ;; The idea here is simple in essence, but incredibly damn annoying...
  ;; Each level of escaping has an extra \. So we want to remove one \ for every
  ;; set of \'s we find.
  (clojure.string/replace string #"(\\+)\"" #(str (apply str (drop-last 1 (%1 1))) "\"")))


  

(defn- parsed-if-parsed [val]
  (when (not (string? val))
    val))


(defn- number-if-number [val]
  (and (string? val)
       (or (re-matches #"^-?[1-9]\d*$" val)
           (re-matches #"^-?[1-9]\d*\.\d+$" val))
       (let [attempt (read-string val)]
         (and (number? attempt)
              attempt))))
           
             

(declare jsonify)

(defn possibly-inner-escaped-data? [string]
  ;; This seems like a strange condition...
  ;; but what we're saying is "if there are 2 or more backslashes
  ;; in a row in this string, we probably need to peel
  ;; things back one level before we try to parse the string
  ;; otherwise, we can just try parsing it.
  ;;
  ;; If there is only one backslash, it's probably escaping
  ;; some quotes that are inside- but if there is more than one,
  ;; that means we probably have additional data inside
  ;; that's further escaped.
  (or (re-find #"\\{2,}" string)
      (re-find #"\\+\"" string)))
  

(defn- array-if-array [val]
  (when (and (string? val) (array-ish? val))
    (if (possibly-inner-escaped-data? val)
      (try
        (vec (map jsonify (parse-string (unescape-one-level val))))
        (catch com.fasterxml.jackson.core.JsonParseException e
          (try
            (vec (map jsonify (parse-string val)))
            (catch com.fasterxml.jackson.core.JsonParseException e
              ;; (log-warn here) maybe? optionally?
              nil))))
      (try
        (vec (map jsonify (parse-string val)))
        (catch com.fasterxml.jackson.core.JsonParseException e
          ;; (log-warn here) maybe? optionally?
          nil)))))
  
(defn- map-if-map [val]
  (when (and (string? val) (map-ish? val))
    (if (possibly-inner-escaped-data? val)
      (try
        (let [base-json (parse-string (unescape-one-level val))]
          (project-map base-json :value-xform jsonify))
          (catch com.fasterxml.jackson.core.JsonParseException e
            (try
              (let [base-json (parse-string val)]
                (project-map base-json :value-xform jsonify))        
              (catch com.fasterxml.jackson.core.JsonParseException e
                ;; (log-warn here) maybe? optionally?
                nil))))
      (try
        (let [base-json (parse-string val)]
          (project-map base-json :value-xform jsonify))        
        (catch com.fasterxml.jackson.core.JsonParseException e
          ;; (log-warn here) maybe? optionally?
          nil)))))

(defn- jsonify [val]
  (or (map-if-map val)
      (array-if-array val)
      (number-if-number val)
      (parsed-if-parsed val)
      val)) ;; else string


(defn parse-json-string [json-string
                          & {:keys [string-transform]
                             :or {string-transform identity}}]
  (project-map (parse-string-with-notify
                 (string-transform json-string))
               :value-xform jsonify))

(defn parse-json-strings [json-strings
                          & {:keys [string-transform]
                             :or {string-transform identity}}]
  (map #(parse-json-string %  :string-transform string-transform) json-strings))
