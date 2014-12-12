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
    (yaml/parse-string
      (slurp filename))
    (catch Exception ex
      (timbre/error "safe-slurp: Cannot locate file: " filename)
      nil)))




(defn write-yaml
  "Serialize a clojure map in yaml formatted file. Returns true on success"
  [filename m]
  (let [weird-marker "JaImE"     ; ugly as hell but works. Empty string is replaced by simple quotes :( TOFIX
        s (string/replace (yaml/generate-string (replace-nils! m weird-marker)) (re-pattern weird-marker) "")]
    (try
      (spit filename s)
      true
      (catch Exception ex
        (timbre/error ex)
        (timbre/error "safe-spit: Cannot write file: " filename)
        nil))))




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
