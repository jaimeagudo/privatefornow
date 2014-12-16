(ns yamlupgrade.util
  (:import jline.Terminal)
  (:import jline.ConsoleReader)
  (:gen-class)
  (:use [clojure.pprint]
        [taoensso.timbre :as timbre :refer (trace debug info warn error report)])
  (:require [clj-yaml.core :as yaml]
            [clojure.data :as data]
            [clojure.string :as string]
            [clojure.java.io :as io]))

;; To hold the default cli-opts values, perhaps could be better to define here
;; all the relevant values rather than on the cl options
(def cli-opts (atom {:options {:verbosity 0}}))
;; The first one is the default strategy
(def CONFLICT_RESOLUTIONS ["interactive" "preserve" "upgrade"])
;; mapping from "-vvv" strings to timbre loglevels
(def verbosity-loglevel
  {0 :info
   1 :debug
   2 :trace})



(defn config-logger!
  "Config the log level among (:trace :debug :info :warn :error :fatal :report)
   If empty it picks up the properties file value or :error "
  ([]
   ;; Too much vvvvvv won't go further than :trace :)
   (config-logger! (get verbosity-loglevel (-> @cli-opts :options :verbosity) :trace )))
  ([loglevel]
    (timbre/set-level! loglevel)
    (timbre/set-config! [:timestamp-pattern] "yyyy-MMM-dd HH:mm:ss")
    (timbre/set-config! [:timestamp-locale] (java.util.Locale/UK))
    (timbre/set-config! [:appenders :spit :enabled?] true)
  ))




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
        (string/replace #"'" ""))))

(defn cast-it!
  "Cast the given string into the right type and return it"
  [s]
  (try
    (case s
      (\newline \return \space \tab "" nil) nil
      "false" false
      "true"  true
      (Integer/parseInt s))
    (catch Exception ex
      (timbre/stacktrace ex)
      s)))


(defn replace-nils!
  "Replace nil values within the given map with empty strings. Useful before serialize"
  [m weird-marker]
  (zipmap (keys m) (map #(cond
                          (nil? %) weird-marker  ; Empty string is replaced by simple quotes :( TOFIX
                          (map? %) (replace-nils! % weird-marker)
                          :else %) (vals m))))



;; Core function to keep comments on the resultant file.

(defn replace-in-template
  "Takes a map of indexes, an ASCII lines vector and pair of key-value to
  replace the current one on the vector. It returns a new ASCII vector with the
  replaced value.  O(1)."
  [keys-index template-vec kv]
  (let [k (first kv)
        index (k keys-index)
        new-ascii-line (yaml/generate-string (apply hash-map kv) :dumper-options {:flow-style :block})]
    (trace "index=" index  "new-ascii-line=" new-ascii-line)
    (assoc template-vec index new-ascii-line)))


;; ## IO System Utilities


(defn read-custom
  "Read a line allowing edition till the user hits enter"
  []
  (binding [*read-eval* false]
    (let [cr (ConsoleReader.)]
      (.readLine cr "custom value (same type as original)="))))



(defn safe-read-char
  "Should be ~safe~ to read a char with this"
  []
  (binding [*read-eval* false]
    (let [term (Terminal/getTerminal)]
      (.disableEcho term)
      (char (.readCharacter term System/in)))))




(defn parse-yaml
  "Slurps a yaml file and returns it content as a map. If not present logs
  error and returns nil."
  [filename]
  (try
    (yaml/parse-string (slurp filename))
    (catch Exception ex
      (error "safe-slurp: Cannot locate file: " filename)
      nil)))



(defn parse-and-index-yaml
  "Takes an index and a yaml-formatted string and returns a map where the key
  is parsed from the string and the value is the given index. Returns nil for
  comments etc."
  [index item]
  (case (first (string/trim item))
    (\# \- \newline \return nil) nil      ; its a comment line or a list item, we could refine it to save comments as well
    (let [m (yaml/parse-string item)
          k (first (keys m))]
      ;;           get ride off the value, we care about the index to provide direct access to the vector position
      (assoc m k index))))




(defn dump!
  "Returns true on success"
  [target-filename s]
  (try
    (info (str "Writing upgraded " target-filename " ..."))
    (spit target-filename s)
    true
    (catch Exception ex
      (error ex)
      (error "spit: Cannot write file: " target-filename)
      nil)))




(defn write-yaml!
  "Writes a .yaml file upgrading the values in the given template-filename with
  the given map. Return true on success"
  ([target-filename new-config]
   ;; As there is no template no comments are possible, just a plain yaml file will be generated
   (let [target-str (yaml/generate-string new-config :dumper-options {:flow-style :block})]
     (dump! target-filename target-str)))
  ([target-filename new-config template-filename]
   (if (nil? template-filename)
     (write-yaml! target-filename new-config)
     (let [;; An array with ASCII text lines
          template (string/split-lines (slurp template-filename))
          ;; We build a template index once O(n) to gain direct access and so constant access time O(1) to make fast replacements
          ;; On this map keys are variables and values are the line numbers within the template file
          template-properties-index (apply merge (keep-indexed parse-and-index-yaml template))
          ;; We partially call replace-in-template with the template-config
          ;; index map as reduce f accepts just 2 arguments
          ascii-vec (reduce (partial replace-in-template template-properties-index) template new-config)
          target-str (string/join "\n" ascii-vec) ]
      (and (try
             ;; We try to re-parse the generated string for validation sake
             (yaml/parse-string target-str)
             (info "Generated .yaml is valid")
             true
             (catch Exception ex
               (error "Generated yaml is not valid, probalby comments messed up things, try safe-mode")
               false))
      (dump! target-filename target-str))))))





(defn backup!
  "Creates a .old backup of the given file, returns true on success."
  [filename]
  (let [source-path filename
        dest-path (str filename ".old")]
    (info "Backing up" dest-path)
    (try
      (nil? (io/copy (io/file source-path) (io/file dest-path)))
      (catch Exception ex
        (error "backup: Error copying file: " filename)
        (timbre/stacktrace ex)))))

(defn exit
  [status msg]
  (info msg)
  ;;   Shutdown timbre agents to avoid delayed process termination
  (shutdown-agents)
  (System/exit status))
