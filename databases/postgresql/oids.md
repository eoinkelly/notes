#OIDs

* The intention of an OID is to be a unique identifier for rows in tables that might not have a primary key. If a table doesn't have a primary key and has OIDs then you can still unambigiously refer to any single row for updating or deleting.
* An OID is a Postgres datatype - see https://www.postgresql.org/docs/current/datatype-oid.html
    * it is an unsigned 4 byte integer (NB: that is not big enough to provide uniqueness on large tables)
* In Postgres 8.0 and earlier, user created tables were created with OIDs enabled. This behaviour is now deprecated and discouraged.
* Object identifiers (OIDs) are used internally by PostgreSQL as primary keys for various system tables


> There are also several alias types for oid: regproc, regprocedure, regoper, regoperator, regclass, regtype, regrole, regnamespace, regconfig, and regdictionary.

> An additional property of most of the OID alias types is the creation of dependencies. If a constant of one of these types appears in a stored expression (such as a column default expression or view), it creates a dependency on the referenced object. For example, if a column has a default expression nextval('my_seq'::regclass), PostgreSQL understands that the default expression depends on the sequence my_seq; the system will not let the sequence be dropped without first removing the default expression