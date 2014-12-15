# yamlupgrade

A [YAML](http://www.yaml.org/) upgrade command line tool intended to upgrade configuration files. It typically accepts a `-n` argument as the new version template reference file with safe defaults and a `-c` argument as the current configuration file to be migrated. Initially dessigned to migrate [cassandra.yaml](https://github.com/apache/cassandra/blob/trunk/conf/cassandra.yaml) files but there is nothing specific about [cassandra](http://cassandra.apache.org/), so it could be used somewhere else.

Inspired by [Datastax](http://www.datastax.com/)

## Installation

Download from http://github.com/jaimeagudo/yamlupgrade

## Usage


    $ java -jar yamlupgrade-0.1.0-standalone.jar -c current.yaml -n target.yaml

## Options

Listing of options this app accepts.

    Usage: yaml-upgrade [options]. By default it will interactively ask to resolve conflicts between current cli-optsuration and the new default values

	Options:
	  -n, --new-yaml TARGET                                        Path to the new default cassandra.yaml
	  -c, --current-yaml CURRENT                                   Path to the current cassandra.yaml
	  -r, --conflicts-resolution CONFLICT-RESOLUTION  interactive  Must be one of ["interactive" "preserve" "upgrade"]. 'interactive' by default
	  -t, --tune-new-options                                       Interactively tune new config options, false by default, safe-defaults provided.
	  -i, --ignore-comments                                        Reduce the chance of error on the migration ignoring any comments, generally safer
	  -v                                                           Verbosity level
	  -h, --help

	Please refer to the manual page for more information.


## Examples


    lr -v -t -c dummy-data/cassandra.1.2.12.yaml -n dummy-data/cassandra.2.0.3.yaml




## License

Copyright © 2014 Jaime Agudo

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
