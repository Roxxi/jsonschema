
##v1.2

###v1.2.2
- MySQL mediumtext, longtext, mediumblob, longblob support

###v1.2.1
- Support Vertica timestamptz, alias to timestamp

###v1.2.0
- Simple type translator
  - Allows for types to be declared with simple strings like
    (e.g. "int", "str")

##v1.1

###v1.1.0
- JSONSchema -> Database Type Mapping
  - Mapping database column types to JSONSchema types with
    appropriate metadata (e.g. min, max)
  - Mapping JSONSchema types to database column types
  - Supported Database Engines: MySQL 5.5, Vertica 6, SQL Server

##v1.0.0
Initial release.
