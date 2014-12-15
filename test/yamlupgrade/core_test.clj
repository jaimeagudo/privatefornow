(ns yamlupgrade.core-test
	(:use midje.sweet
        yamlupgrade.util
        yamlupgrade.core))



(facts "Testing cache-exists?"
       (let [options {:current-yaml "dummy-data/cassandra.1.2.12.yaml"
                      :new-yaml "dummy-data/cassandra.2.0.3.yaml"}
             current-config (parse-yaml (:current-yaml options))
             new-config (parse-yaml (:new-yaml options))
             pre-options (assoc options :conflict-resolution (nth CONFLICT_RESOLUTIONS 1))
             up-options (assoc options :conflict-resolution (nth CONFLICT_RESOLUTIONS 2))]
         (fact "Test files exist and are correctly parsed"
               (or (nil? current-config) (nil? new-config)) => false)
         (fact "Upgrade conflict resolution approach works, there is nothing to add"
               (confirm-edit-script! current-config new-config up-options) =>
               {:inter_dc_tcp_nodelay false, :read_request_timeout_in_ms 5000, :write_request_timeout_in_ms 2000})
         (fact "Preserve conflict resolution approach works, there is nothing to add"
               (confirm-edit-script! current-config new-config pre-options) =>
               {:inter_dc_tcp_nodelay true, :read_request_timeout_in_ms 10000, :write_request_timeout_in_ms 10000})
        )
       )
