
# Vault

A secrets management application

* has a server and client in a single go binary
* server exposes a HTTP API
* client interacts with the HTTP API
* server stores secrets in one of many "backends" e.g. in-memory, file on disk,
  some enterprise secrets management solution
* backends are mounted at URLs e.g. a "generic" backend might be mounted at
  `/secret`
* dynamic secrets
    * secrets do not have to be files stored on disk
    * secrets do not have to exist before they are read
    * an AWS backend can read/write IAM policies and access tokens
    * a SQL backend might read/write data from your enterprise's secrets DB
* vault can be clever about automatically generating secrets for you when you
  "read" from a particular backend e.g. the AWS backend might query IAM to get
  a temporary credentials for your script
* authentication
    * vault has pluggable auth backends e.g.
        * private key
        * github based
