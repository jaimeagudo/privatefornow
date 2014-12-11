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

(defn replace-nils!
  "Replace nil values within the given map with empty strings. Useful before serialize"
  [m weird-marker]
  (zipmap (keys m) (map #(cond
                          (nil? %) weird-marker  ; Empty string is replaced by simple quotes :( TOFIX
                          (map? %) (replace-nils! % weird-marker)
                          :else %) (vals m))))



(defn- parse-yaml
  "Slurps a yaml file and returns it content as a map. If not present logs error and returns nil."
  [filename]
  (try
    (yaml/parse-string
      (slurp filename))
    (catch Exception ex
      (timbre/error "safe-slurp: Cannot locate file: " filename)
      nil)))

(defn- write-yaml
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

; (defn url-escape-str "doc-string" [string]
;   (java.net.URLEncoder/encode string))

;; (defn format-date
;;   ([](format-date (date) "yyyy MM dd HH mm ss"))
;;   ([x](if (string? x)
;;         (format-date (date) x)
;;         (format-date x "yyyy MM dd HH mm ss")))
;;   ([dt fmt](.format (SimpleDateFormat. fmt) dt)))


(defn resolve-conflicts
  "Interactively build and return a map with the chosen values for the conflicting keys"
  [current-config new-config conflict-keys]
  (println "Please choose to (k)eep the current value, (u)pgrade to new version safe default value or a (c)ustom one")
  (println "--------------------------------------------------------------------------------------------------------")
;;   (log "conflicting keys=>")
;;   (log  conflict-keys)
  (let [values
          (for [k conflict-keys
                :let [current (k current-config)
                      new (k new-config)]]
            (loop []
              (println (str (name k) "=" current "(k)  " new "(u)  <<CUSTOM>>(c)"))
              (let [chosen (case (first (safe-read))
                             \k current
                             \u new
                             \c (sanitize (safe-read))
                             nil)]
                (if (nil? chosen)
                  (recur)
                  chosen))))]
            (log "values")
            (log values)
            (zipmap conflict-keys values)))




(defn customize-map
  "For each key in the given map ask to keep its current value or a custom one. It retuns the resultant map"
  [m]
  (println "Please choose to (k)eep the current safe default value or provide a (c)ustom one")
  (println "--------------------------------------------------------------------------------")
  (let [ks (keys m)
        values
            (for [k ks
                  :let [v (k m)]]
              (loop []
                (println (str (name k) "=" v "(k)  <<CUSTOM>>(c)"))
                (let [chosen (case (first (safe-read))
                               \k v
                               \c (sanitize (safe-read))
                               nil)]
                  (if (nil? chosen)
                    (recur)
                    chosen))))]
    (log "values")
    (log values)
    (zipmap ks values)))



(defn upgrade-config!
  "Upgrades current-config to new a version with the given safe defaults as base and guided by the given options.
  Takes and returns Clojure maps, .yaml agnostic"
  [current-config new-config options]
    (let [edit-script (second (m/diff current-config new-config))
          base-config
          (if (empty? (:r edit-script))
            new-config
            (case (:conflict-resolution options)
              "preserve"     (merge new-config (select-keys current-config (:r edit-script)))
              "upgrade"     new-config ; just take new safe-defaults from new config
              "interactive"  (merge new-config (resolve-conflicts current-config new-config (map first (:r edit-script))))))]

;;       (log edit-script)
;;       (log base-config)

      (when (seq (:- edit-script))
        (timbre/warn "The following options found in your current .yaml file are DEPRECATED and so REMOVED from your new configuration\newline"
                     (map #(str (name %) \newline)   (:- edit-script))))

      (if (seq (:+ edit-script))
        (if (:tune options)
          (merge base-config (customize-map (apply assoc {} (flatten (:+ edit-script)))))
          (do
            (timbre/info "The following options are NEW, current values are safe defaults added to your new configuration\newline"
                         (map #(str (name (first %)) \newline) (:+ edit-script)))
            base-config)
          ))))


(defn- backup!
  "Creates a .old backup of the given file, returns true on success."
  [filename]
  (try
    ; could use mv perhaps
    (clojure.java.shell/sh (str "cp -f " (:current-yaml options) " " (:current-yaml options) ".old"))
    true
  (catch Exception ex
        (timbre/error ex)
        (timbre/error "safe-spit: Cannot write file: " filename)
        nil)))



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
    ;; Backup current config
    (backup! (:current-yaml options))
    (let [result-config (upgrade-config! current-config new-config options)]
     (log result-config)
     (log "Writing yaml... ")
     (write-yaml "result.yaml" result-config)
;;      (write-yaml (:current-yaml options result-config))
     (log "Done!")
     )))
