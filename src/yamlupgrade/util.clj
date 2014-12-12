(ns yamlupgrade.util
  (:import jline.Terminal)
  (:import jline.ConsoleReader)
  (:gen-class)
  (:use [clojure.pprint]
        [clojure.java.shell :only [sh]]
        [taoensso.timbre :as timbre :refer (trace debug info warn error report)])
  (:require [clj-yaml.core :as yaml]
            [clojure.data :as data]
            [clojure.string :as string]))

;; To hold the default cli-opts values, perhaps could be better to define here all the relevant values rather than on the cl options

(def cli-opts (atom {:options {:verbosity 0}}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sanitize
  "Sanitize string to remove prohibited chars"
  [s]
  (let [CONTROL_CHARS (apply str (map char (conj (range 32) 127)))
        RESERVED_CHARS "$&+,/:;=?@"
        UNSAFE_CHARS "\"<>#%\\{\\}\\|\\^~\\[\\]`"
        EXTRA_CHARS "\\(\\)!\\\\-"
        BLACKLIST_PATTERN (re-pattern (str "[" CONTROL_CHARS RESERVED_CHARS UNSAFE_CHARS EXTRA_CHARS "]"))]
    (-> s string/lower-case
        (string/replace BLACKLIST_PATTERN " ")
        string/trim
        ;;         (s/replace #"[\s]+" "-")
        (string/replace #"'" ""))))


; TODO write as macro
(defn log
  "Pretty logging of the given argumetn based on global verbosity value
  (it might log nothing if verbosity is 0)"
  [o]
  (if (pos? (-> @cli-opts :options :verbosity))
    (timbre/log (pprint o))))


(defn error-msg [errors]
  (timbre/error "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))



(defn exit [status msg]
  (timbre/info msg)
  (System/exit status))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn cast-it!
  [o]
  (try
    (case o
      "false" false
      "true"  true
      (Integer/parseInt o))
    (catch Exception ex
;;       (timbre/error ex)
      o)))



(defn replace-nils!
  "Replace nil values within the given map with empty strings. Useful before serialize"
  [m weird-marker]
  (zipmap (keys m) (map #(cond
                          (nil? %) weird-marker  ; Empty string is replaced by simple quotes :( TOFIX
                          (map? %) (replace-nils! % weird-marker)
                          :else %) (vals m))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; IO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn safe-read
  "Should be ~safe~ to read a char with this"
  []
  (binding [*read-eval* false]
    (let [cr (ConsoleReader.)]
      (.readLine cr "custom="))))

(defn safe-read-char
  "Should be ~safe~ to read a char with this"
  []
  (binding [*read-eval* false]
    (let [term (Terminal/getTerminal)]
      (.disableEcho term)
      (char (.readCharacter term System/in)))))




(defn parse-yaml
  "Slurps a yaml file and returns it content as a map. If not present logs error and returns nil."
  [filename]
  (try
    (yaml/parse-string (slurp filename))
    (catch Exception ex
      (timbre/error "safe-slurp: Cannot locate file: " filename)
      nil)))

 (defn replace-in-template
    "Takes a map of values to be applied on the given ASCII lines vector and pair of key-index for fast replacement. O(1).
    It returns a new ASCII vector with the new value"
    [new-values-map template-vec key-index]
    (let [kw (first key-index)
          index (second key-index)
          new-ascii-line (yaml/generate-string (select-keys new-values-map kw))]
      (log kw)
      (log index)
      (log new-ascii-line)
      (assoc template-vec index new-ascii-line)))




  (defn parse-and-index-yaml
    "Takes an index and a yaml-formatted string and returns a map where the key
    is parsed from the string and the value is the given index. Returns nil for
    comments etc."
    [index item]
    (println "JUst one item~~~~~~~~")
    (log item)
    (case (first (string/trim item))
      (\# \-) nil      ; its a comment line or a list item, we could refine it to save comments as well
      (let [m (yaml/parse-string item)
            j (println "m=" m)
            k (first (keys m))]
        ;;           get ride off the value, we care about the index to provide direct access to the vector position
        (assoc m k index))))


  (defn write-file!
    "Returns true on success"
    [target-filename s]
    (try
      (spit target-filename s)
      true
      (catch Exception ex
        (timbre/error ex)
        (timbre/error "spit: Cannot write file: " target-filename)
        nil)))



(defn backup!
  "Creates a .old backup of the given file, returns true on success."
  [filename]
  (try
    ; could use mv perhaps
    (let [command (str "cp -f " filename " " filename ".old")]
      (log (sh "pwd")) ; see :dir http://clojuredocs.org/clojure.java.shell/sh TODO
      (log command)
      (= 0 (:exit (sh command))))
  (catch Exception ex
        (timbre/error ex)
        (timbre/error "backup: Cannot write file: " filename)
        nil)))





(defn write-yaml
  "Serialize a clojure map in yaml formatted file. It accepts an optional template-filename with comments. Returns true on success"
  ([target-filename new-config]
   (let [weird-marker "JaImE"     ; ugly as hell but works. Empty string is replaced by simple quotes :( TOFIX
         s (string/replace (yaml/generate-string (replace-nils! new-config weird-marker)) (re-pattern weird-marker) "")]
     (write-file! target-filename s)))
  ([target-filename new-config template-filename]
    (let [tf (slurp template-filename)
          ;; An array with ASCII text lines
          template  (string/split-lines tf)
          ;; We build a template index once O(n) to gain direct access and so constant access time O(1) to make fast replacements
          ;; On this map keys are variables and values are the line numbers within the template file
          template-config-index (apply merge (keep-indexed parse-and-index-yaml template))
          ;; We partially call replace-in-template with the new-config map as
          ;; reduce f accepts just 2 arguments and its value won't change
          ascii-vec (reduce (partial replace-in-template new-config) template template-config-index)
          target-str (string/replace (string/join ascii-vec \n) (re-pattern "null") "")]
      (write-file! target-filename target-str))))
