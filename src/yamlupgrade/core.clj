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

;; To hold the default cli-opts values, perhaps could be better to define here
;; all the relevant values rather than on the cl options
(def cli-opts (atom {:options {:verbosity 0}}))

(defn safe-read
  "Should be ~safe~ to read a char with this"
  []
  (binding [*read-eval* false]
    (read-line)))


; write as macro
(defn- log
  "Pretty logging of the given argumetn based on global verbosity value
  (it might log nothing if verbosity is 0)"
  [o]
  (if (pos? (-> @cli-opts :options :verbosity))
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
    ]
   ["-c" "--current-yaml CURRENT" "Path to the current cassandra.yaml"
    :default "resources/cassandra.1.2.12.yaml"
    ]
    ;; If no required argument description is given, the option is assumed to be a boolean option defaulting to nil
   ["-r" "--conflicts-resolution CONFLICT-RESOLUTION" "Must be on of 'preserve' 'upgrade' 'interactive'. 'interactive' by default"
    :id :conflict-resolution
    :default "interactive"
    :validate [#(some #{%} ["preserve" "upgrade" "interactive"]) "Must be one of 'preserve' 'upgrade' 'interactive'" ]
    ]
   ["-t" "--tune-new-options" "Interactively tune new config options, false by default, safe-defaults provided."
    :id :tune
    ]
   ;; A non-idempotent option
   ["-v" nil "Verbosity level"
    :id :verbosity
    :default (-> @cli-opts :options :verbosity)
    :assoc-fn (fn [m k _] (update-in m [k] inc))]
   ;; A boolean option defaulting to nil
   ["-h" "--help"]])


(defn usage [options-summary]
  (->> ["cassandra.yaml upgrade tool."
        ""
        "Usage: program-name [options]. By default it will interactively ask to resolve conflicts between current cli-optsuration and the new default values"
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sanitize [s] s) ; TODO


(defn resolve-conflicts
  "Interactively build and return a map with the chosen values for the conflicting keys"
  [current-config new-config conflict-keys]
  (println "Please choose to (k)eep the current value, (u)pgrade to new version safe default value or a (c)ustom one")
  (println "--------------------------------------------------------------------------------------------------------")
  (log conflict-keys)
  (zipmap conflict-keys
          (for [k conflict-keys
                :let [current (k current-config)
                      new (k new-config)]]
                (loop []
                  (if (nil?
                       (try
                         (println (str (name k) "=" current "(k)  " new "(u)  <<CUSTOM>>(c)"))
                         (case (first (safe-read))
                           \k current
                           \u new
                           \c (sanitize (safe-read))
                           )
                         (catch Exception ex
                           (timbre/error ex)
                           nil)))
                  (recur))))))




(defn customize-defaults
  "Interactively build and return a map with the chosen values for the conflicting keys"
  [current-config new-keys]
  (println "Please choose to (k)eep the current safe default value or provide a (c)ustom one")
  (println "--------------------------------------------------------------------------------------------------------")
  (log new-keys)
  (zipmap new-keys
          (for [k new-keys
                :let [current (k current-config)]]
                (loop []
                  (if (nil?
                       (try
                         (println (str (name k) "=" current "(k)  <<CUSTOM>>(c)"))
                         (case (first (safe-read))
                           \k current
                           \c (sanitize (safe-read))
                           )
                         (catch Exception ex
                           (timbre/error ex)
                           nil)))
                  (recur))))))




(defn -main [& args]
  "Parse options and launch upgrade process"
  (reset! cli-opts (parse-opts args cli-options))
  (let [{:keys [options arguments errors summary]} @cli-opts
        current-config (parse-yaml (:current-yaml options))
        new-config (parse-yaml (:new-yaml options))]
    ;; Handle help and error conditions
    (log options)
    (cond
     (:help options) (exit 0 (usage summary))
     ;;       (not= (count arguments) 1) (exit 1 (usage summary))
     (or (nil? current-config) (nil? new-config)) (exit 1)
     errors (exit 1 (error-msg errors)))
    (let [edit-script (second (m/diff current-config new-config))
          result-config
          (if (seq (:- edit-script))
            (do
              (log edit-script)
              (timbre/warn "The following options found in your current .yaml file are DEPRECATED, they will REMOVED from your new configuration\newline"
                         (map #(str (name %) \newline)   (:- edit-script)))
              (dissoc current-config (:- edit-script)))
            new-config)
          base-config
          (if (seq (:r edit-script))
            (case (:conflict-resolution options)
              "preserve"     (merge result-config (select-keys current-config (:r edit-script)))
              "upgrade"      (merge result-config (select-keys new-config (:r edit-script)))
              "interactive"  (merge result-config (resolve-conflicts current-config new-config (map first (:r edit-script))))
              )
            result-config)]
      (if (seq (:+ edit-script))
        (if (:tune options)
          (merge base-config (customize-defaults new-config (:+ edit-script)))
          (do
            (timbre/warn "The following options are NEW, current values are safe defaults added to your new configuration\newline"
                         (map #(str (name %) \newline) (:+ edit-script)))
            base-config)
          )))))


;;      (println "OLD THINGS")
;;      (pprint (walk/stringify-keys old-things))
;;      (println "NEW THINGS")
;;      (pprint (walk/stringify-keys new-things))
