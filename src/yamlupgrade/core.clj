(ns yamlupgrade.core
  (:gen-class)
  (:use [yamlupgrade.util]
        [clojure.pprint]
        [taoensso.timbre :as timbre :refer (trace debug info warn error report)])
  (:require [clj-yaml.core :as yaml]
            [clojure.string :as string]
            [diffit.map :as m]
            [clojure.tools.cli :refer [parse-opts]]))



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
       (string/join \n)))




(defn resolve-conflicts
  "Interactively build and return a map with the chosen values for the conflicting keys"
  [current-config new-config conflict-keys]
  (println "Please choose to (k)eep the current value, (u)pgrade to new version safe default value or a (c)ustom one")
  (println "--------------------------------------------------------------------------------------------------------")
  (trace "conflicting keys"  conflict-keys)
  (let [values
          (for [k conflict-keys
                :let [current (k current-config)
                      new (k new-config)]]
            (loop []
              (println (str (name k) "=" current "(k)  " new "(u)  <<CUSTOM>>(c)"))
              (let [chosen (case (safe-read-char)
                             \k current
                             \u new
                             \c (cast-it! (sanitize (safe-read)))
                             nil)]
                (if (nil? chosen)
                  (recur)
                  chosen))))]
            (trace "resolved conflicts" values)
            (zipmap conflict-keys values)))




(defn customize-map
  "For each key in the given map ask to keep its current value or a custom one. It retuns the resultant map"
  [m]
  ;; TODO generate just changed ones
  (println "Please choose to (k)eep the current safe default value or provide a (c)ustom one")
  (println "--------------------------------------------------------------------------------")
  (let [ks (keys m)
        values
            (for [k ks
                  :let [v (k m)]]
              (loop []
                (println (str (name k) "=" v "(k)  <<CUSTOM>>(c)"))
                (let [chosen (case (safe-read-char)
                               \k v
                               \c (cast-it! (sanitize (safe-read)))
                               nil)]
                  (if (nil? chosen)
                    (recur)
                    chosen))))]
    (trace "customized values" values)
    (zipmap ks values)))




(defn confirm-edit-script!
  "Confirms with user the edit-script between the given configs to reach
  new-config from current-config. Takes and returns Clojure maps,
  .yaml agnostic"
  [current-config new-config options]
    (let [edit-script (second (m/diff current-config new-config))
          resolved-conflicts-edit-script
          (if (empty? (:r edit-script))
            {}
            (case (:conflict-resolution options)
              "preserve"     (select-keys current-config (:r edit-script))
              "upgrade"      (select-keys new-config (:r edit-script)); just take new safe-defaults from new config
              "interactive"  (resolve-conflicts current-config new-config (map first (:r edit-script)))))]

      (when (seq (:- edit-script))
        (info "The following options found in your current .yaml file are DEPRECATED and so REMOVED from your new configuration\n"
                     (map #(str (name %) \n)   (:- edit-script))))

      (if (seq (:+ edit-script))
        (if (:tune options)
          ;; TODO customize-map could be optimised to return just non-defaulted values
          (merge resolved-conflicts-edit-script (customize-map (apply assoc {} (flatten (:+ edit-script)))))
          (do
            (info "The following options are NEW, current values are safe defaults added to your new configuration\n"
                         (map #(str (name (first %)) \n) (:+ edit-script)))
            resolved-conflicts-edit-script)
          ))))




(defn -main
  [& args]
  "Parse options and launch upgrade process"
  (reset! cli-opts (parse-opts args cli-options))
  (config-logger!)
  (let [{:keys [options arguments errors summary]} @cli-opts
        current-config (parse-yaml (:current-yaml options))
        new-config (parse-yaml (:new-yaml options))]
;; Handle help and error conditions
;; (trace options)
    (cond
     (:help options) (exit 0 (usage summary))
     ;;       (not= (count arguments) 1) (exit 1 (usage summary))
     (or (nil? current-config) (nil? new-config)) (exit 1)
     errors (exit 1 (error "The following errors occurred while parsing your command:\n\n"
                         (string/join \newline errors))))
;; Backup current config
;;     (backup! (:current-yaml options))
    (let [result-config (confirm-edit-script! current-config new-config options)]
      (trace result-config)
      (info "Writing yaml... ")
      (write-yaml! "result.yaml" result-config (:new-yaml options)))
    (info "Done!")
;;   Shutdown timbre agents
    (shutdown-agents)
    ))
