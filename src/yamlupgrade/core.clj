(ns yamlupgrade.core
  (:gen-class)
  (:use [clojure.pprint]
        [taoensso.timbre :as timbre :refer (trace debug info warn error report)])
  (:require [clj-yaml.core :as yaml]
            [clojure.data :as data]
            [clojure.walk :as walk]
            [clojure.string :as string]
            [diffit.map :as m]
            [clojure.tools.cli :refer [parse-opts]]))

;; To hold the default config values, perhaps could be better to define here
;; all the relevant values rather than on the cl options
(def config (atom {:options {:verbosity 0}}))


; write as macro
(defn- log
  "Pretty logging of the given argumetn based on global verbosity value
  (it might log nothing if verbosity is 0)"
  [o]
  (if (pos? (-> @config :options :verbosity))
    (timbre/log (pprint o))))


(defn- parse-yaml
  "Slurps a yaml file and returns it content as a map. If not present logs error and returns nil."
  [filename]
  (try
    (yaml/parse-string
      (slurp filename))
    (catch Exception ex
      (timbre/error "safe-slurp: Cannot locate file: " filename)
      nil)))


;; (yaml/parse-string "
;; - {name: John Smith, age: 33}
;; - name: Mary Smith
;;   age: 27
;; ")

(def cli-options
  ;; An option with a required argument
  [["-n" "--new-yaml TARGET" "Path to the new default cassandra.yaml"
    :default "resources/cassandra.2.0.3.yaml"
;;     :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]] TODO add reg-exp with *.yaml
    ]
   ["-c" "--current-yaml CURRENT" "Path to the current cassandra.yaml"
    :default "resources/cassandra.1.2.12.yaml"
;;     :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]  TODO add reg-exp with *.yaml
    ]
   ;; If no required argument description is given, the option is assumed to be a boolean option defaulting to nil
   ["-r" "--conflicts-resolution" "Fully automatic mode will use safe-defaults values on conflicts"]
;;     :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]  TODO add reg-exp with *.yaml
    ]
   ["-pr" "--preserve-conflicts-resolution" "Fully automatic mode will preserve current values on conflicts"]
;;     :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]  TODO add reg-exp with *.yaml
    ]
  [ "-m" "--manual-tunning" "Will interactively ask custom values for all the new configuration options"]
   ;; A non-idempotent option
   ["-v" nil "Verbosity level"
    :id :verbosity
    :default (-> @config :options :verbosity)
    :assoc-fn (fn [m k _] (update-in m [k] inc))]
   ;; A boolean option defaulting to nil
   ["-h" "--help"]])


(defn usage [options-summary]
  (->> ["cassandra.yaml upgrade tool."
        ""
        "Usage: program-name [options]. By default it will interactively ask to resolve conflicts between current configuration and the new default values"
        ""
        "Options:"
        options-summary
        "Please refer to the manual page for more information."]
       (string/join \newline)))

(defn error-msg [errors]
  (timbre/error "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn exit [status msg]
  (timbre/info msg)
  (System/exit status))


(defn -main [& args]
  "Parse options and launch upgrade process"
  (reset! config (parse-opts args cli-options))
  (let [{:keys [options arguments errors summary]} @config
        current-config (parse-yaml (:current-yaml options))
        new-config (parse-yaml (:new-yaml options))]
    ;; Handle help and error conditions
    (log options)
    (cond
     (:help options) (exit 0 (usage summary))
     ;;       (not= (count arguments) 1) (exit 1 (usage summary))
     (or (nil? current-config) (nil? new-config)) (exit 1)
     errors (exit 1 (error-msg errors)))
    (let [edit-script (second (m/diff current-config new-config))]
      (if-not (empty?      (:- edit-script))
        (timbre/warn "The following options found in your current .yaml file are deprecated, they will be IGNORED by the new software version"
                     (map #(str (name %) \newline)   (:- edit-script))))
      (if-not (:apply-safe-defaults options)

;;       (log edit-script)
    )))
;;      (println "OLD THINGS")
;;      (pprint (walk/stringify-keys old-things))
;;      (println "NEW THINGS")
;;      (pprint (walk/stringify-keys new-things))
