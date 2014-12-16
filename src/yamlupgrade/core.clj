(ns yamlupgrade.core
  (:gen-class)
  (:use [yamlupgrade.util]
        [clojure.pprint]
        [taoensso.timbre :as timbre
         :refer (trace debug info warn error report)])
  (:require [clj-yaml.core :as yaml]
            [clojure.string :as string]
            [diffit.map :as m]
            [clojure.tools.cli :refer [parse-opts]]))

;; ##Command line auxiliary stuff

(def cli-options
  ;; Options are treated as arguments, TOFIX
   ;; An option with a required argument
  [["-n" "--new-yaml TARGET" "Path to the new default cassandra.yaml"]
   ;; An option with a required argument
   ["-c" "--current-yaml CURRENT" "Path to the current cassandra.yaml"]
    ;; If no required argument description is given, the option is assumed to be a boolean option defaulting to nil
   ["-r" "--conflicts-resolution CONFLICT-RESOLUTION" (str "Must be one of " CONFLICT_RESOLUTIONS ". 'interactive' by default")
    :id :conflict-resolution
    :default (nth  CONFLICT_RESOLUTIONS 0)
    :validate [#(some #{%} CONFLICT_RESOLUTIONS) (str "Must be one of " CONFLICT_RESOLUTIONS) ]]
   ["-t" "--tune-new-options" "Interactively tune new config options, false by default, safe-defaults provided."
    :id :tune ]
   ["-i" "--ignore-comments" "Reduce the chance of error on the migration ignoring any comments, generally safer"]
;;    ["-s" "--add-signature" "Add a generation signature with timestamp"] ; TODO
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
        "Usage: yaml-upgrade [options]. By default it will interactively ask to resolve conflicts between current cli-optsuration and the new default values"
        ""
        "Options:"
        options-summary
        "\nPlease refer to the manual page for more information."]
       (string/join \newline)))


;; ## User interaction to resolve conflicts

(defn resolve-conflicts
  "Interactively build and return a map with the chosen values for the
  conflicting keys"
  [current-config new-config conflict-keys]

  (info "The following properties have different values in your current and the new version safe defaults")
  (info conflict-keys)
  (println "Please choose to (k)eep the current value, (u)pgrade to new version safe default value or a (c)ustom one")
  (println "--------------------------------------------------------------------------------------------------------")
  (let [values
          (for [k conflict-keys
                :let [current (k current-config)
                      new (k new-config)]]
            (loop []
              (println (str (name k) "=" current "(k)  " new "(u)  <<CUSTOM>>(c)"))
              (let [chosen (case (safe-read-char)
                             \k current
                             \u new
                             \c (cast-it! (sanitize (read-custom)))
                             nil)]
                (if (or (nil? chosen) (not (= (type chosen) (type new)))) ;;
                  (recur)
                  (do
                    (println "=>"(name k) "=" chosen)
                  chosen)))))]
            (trace "resolved conflicts" values)
            (zipmap conflict-keys values)))


;; ## User interaction to tune defaults for new properties

(defn customize-map
  "For each key in the given map ask to keep its current value or a custom one.
  It retuns the resultant map"
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
                (flush) ; needed whith increased loglevel
                (let [chosen (case (safe-read-char)
                               \k v
                               \c (cast-it! (sanitize (read-custom)))
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
          unused (trace "Edit-script=" edit-script)
          conflict-keys (map first (:r edit-script))
          resolved-conflicts-edit-script
          (if (empty? (:r edit-script))
            {}
            (case (:conflict-resolution options)
              "interactive"  (resolve-conflicts current-config new-config conflict-keys)
              "preserve"     (select-keys current-config conflict-keys)
              "upgrade"      (select-keys new-config conflict-keys)))]; just take new safe-defaults from new config
      (when (seq (:- edit-script))
        (info "The following options found in your current .yaml file are DEPRECATED and so REMOVED from your new configuration\n"
              (map #(str (name %) \n) (:- edit-script))))

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
        {:keys [current-yaml new-yaml]} options]
    ;; Handle help and error conditions
    (println "(:ignore-comments options)" (:ignore-comments options))
    (trace options)
    (cond
     (:help options) (exit 0 (usage summary))
     (or (nil? current-yaml) (nil? new-yaml)) (exit 1 (usage summary))
     errors (exit 1 (str "The following errors occurred while parsing your command:\n\n"
                           (string/join \newline errors))))
    (let [current-config (parse-yaml current-yaml)
          new-config (parse-yaml new-yaml)]
          (if (or (nil? current-config) (nil? new-config))
            (exit 1 "Cannot load file/s")
            (let [result-config (confirm-edit-script! current-config new-config options)
                  template-file (if-not (:ignore-comments options) new-yaml)]
              (trace (str "result-config= "result-config))
              ;; Backup current config
              (backup! current-yaml)
              (if (write-yaml! current-yaml result-config template-file)
                (exit 0 "Done!")
                (exit 1 "Failed!"))
            )))))
