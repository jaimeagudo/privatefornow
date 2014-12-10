(ns yamlupgrade.core
  (:gen-class)
  (:use [clojure.pprint]
        [taoensso.timbre :as log :refer (trace debug info warn error report)])
  (:require [clj-yaml.core :as yaml]
            [clojure.data :as data]
            [clojure.walk :as walk]
            [diffit.map :as m]))


;; (yaml/generate-string
;;   [{:name "John Smith", :age 33}
;;    {:name "Mary Smith", :age 27}])
;; "- {name: John Smith, age: 33}\n- {name: Mary Smith, age: 27}\n"


(defn parse-yaml
  "Slurps a yaml file and returns it content as a map. If not present logs error and returns nil."
  [filename]
  (try
    (yaml/parse-string
      (slurp filename))
    (catch Exception ex
      (log/error "safe-slurp: Cannot locate file: " filename)
      nil)))


;; (yaml/parse-string "
;; - {name: John Smith, age: 33}
;; - name: Mary Smith
;;   age: 27
;; ")


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
   (let [old-yaml (parse-yaml "resources/cassandra.1.2.12.yaml")
         new-yaml (parse-yaml "resources/cassandra.2.0.3.yaml")
         differences (data/diff old-yaml new-yaml)
         old-things (nth differences 0) ;things only in old
         new-things (nth differences 1) ;things only in new
         common-things (nth differences 2)
         ]
     (println "OLD THINGS")
     (pprint (walk/stringify-keys old-things))
     (println "NEW THINGS")
     (pprint (walk/stringify-keys new-things))
     (print "patch approach")
     (pprint (m/diff old-yaml new-yaml))))
