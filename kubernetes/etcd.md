# etcd

Sources

- https://www.youtube.com/watch?v=jnfgqUzubKk
    - overview and history
- https://www.youtube.com/watch?v=GJqO1TYzVDE
    - Q&A with etcd maintainers
- https://www.youtube.com/watch?v=O_ap8R3HbV4
    - how etcd uses Raft
- https://www.youtube.com/watch?v=NVMZBBQ9hsM
    - Debugging etcd

Background

- has been around for 5+ years
- is a key value store
    - complex values are encoded as JSON/Protobuf/YAML/whatever
- designed for storing relatively small amounts of data
- stores all values in memory with the bolt db file `mmap`ed into memory
- distributed (typically 3 or 5 nodes)
- Consistent + Partition Tolerant + (Highly) Available in CAP theorem
    - Strong(sequential) consistency (NOT eventual consistency)
- Related systems
    - Google Chubby
    - Apache Zookeeper
- Properties:
    - highly available
    - strong consistency model
    - scalable watch mechansim
    - concurrency control primitves
- uses the RAFT consensus algorithm
- exposes metrics via a Prometheus endpoint `/metrics` on each node
- Uses BoltDB as the backing store for persisting data to disk
- uses GRPC API since v3
    - The high-performance binary GRPC API is "re-exposed" as JSON+HTTP
      endpoints
- The _key space_
    - The "key space" (set of all keys) is divided up via `/` separators (TODO:
      I'm not sure whether this is just a convention)
        - the key space is MVCC (multi-version concurrency control) i.e. old
          versions are kept
        - values are retrieved by key+version
        - Applications should choose their keyspace carefully because etcd
          allows you to do _range queries_ which will query across a lexically
          ordered subset of the keyspace
            - careful choice of keyspace lets you cut with the grain of this
              feature e.g. you can get all keys with a common prefix
- Common operations:
    - `RANGE <start-key>..<end-key>`
    - `DELETE RANGE <start-key>..<end-key>`
    - `PUT <key> <value>`
    - `TXN (if <condition> then <op1, op2 ...> else <op3, op4, ...>)`
        - TXN is multipurpose:
            - you can use it as a "check and set" operation e.g. "only do this
              write if the value I expect exists"
            - if you hardwire `<condition>` to be true then you can do multiple
              operations atomically
    - `CREATE WATCH <key1>..<key2>`
        - a streaming operation
        - establishes a GRPC bidirectional stream with the server
        - can establish watches of regsions of the keyspace
        - is eventually consistent
- All operations are _linearizably consistent_ aka _externally consistent_ i.e.
  they are consistent from the POV of an external client/observer
    - if a client does a PUT to member A of a cluster and then reads the same
      key from member B then it is guaranteed to see the value it just wrote
- Implements _Multi-version concurrency control_ at two levels:
    1. The etcd keyspace implements MVCC
    1. The underlying BoltDB also implements MVCC
- Is Copy-on-write for all modifications - it does **not** do "in place" updates
  of objects
- Compaction
    - applies to the etcd keyspace (not the BoltDB)
    - can happen automatically
    - can be taken care of by the client e.g. k8s manually runs compaction every
      5 mins
    - Kubernetes requests compaction automatically - every 5 mins it requests
      all versions older than 5 mins are removed
        - => kubernetes doesn't have much history of its config available
- Defragmentationa applies at the boltDB file level
    - it recovers all free space in the bolt db file
    - etcd will only defrag the file when requested
    - Defragging is a "stop the world" operation
- `etcd/raft` is a go package used in other projects too e.g. CockroachDB
- the _list and watch_ pattern is key to the design of Kubernetes
- is available as an operator
    - TODO: learn more about operators
- has a bootstrapping service at `discovery.etcd.io`
- Reads:
    - A client can request data from any instance and get the same answer
- Writes
    - The elected leader is responsible for processing writes
    - If a write comes in to a follower node then they forward the write to the
      leader internally and then the leader takes care of distribution the new
      revision of the database to all nodes
    - If a write comes in and some nodes are unreachable in the cluster then the
      write is considered to succeed if it was successfully written to the the
      **majority** (or quorum) of nodes in the cluster
- Quorum = `N/2 + 1`
    - The min number of nodes which must be available to do successful writes
    - in the `N/2 + 1` formula, if there is any decimal place just remove it
    - If you have an even number of nodes then there is a greater possibility of
      the cluster failing if the network segments in a way to leave the same
      number of nodes on each side
- The leader
    - sends out notifications regularly to other nodes asserting that it is
      still the leader
    - if these heartbeats are missed the other nodes will start a re-election
- The `--initial-cluster` param is how etcd knows it is part of a cluster

- etcd has the concept of users (with passwords) and roles
    - https://github.com/etcd-io/etcd/blob/master/Documentation/op-guide/authentication.md
    - TODO: I'm not sure k8s uses this at all - OpenShift doesn't seem to at
      least
- There are 3 dimensions of storage in the DB
    1. how many objects in the DB
    2. how big is each object
    3. how many **uncompacted** revisions of each object are there Keep that in
       mind when trying to figure out why the DB is big
- How is cluster version decided?
    1. Leader gets server version from each peer
    2. Leader picks the lowest version as the _cluster version_
    3. Leader broadcasts it to all peers
    4. Each peer tries to apply that cluster version
- If a node with an older version than the cluster version tries to join it will
  fail _ => If you want to upgrade your server, you must do it incrementally,
  one by one (rolling upgrade) _ etcd 3.4 adds an ability to temporarily
  whitelist a downgraded version
- You can use SRV DNS records to tell an etcd node how to bootstrap itself
- etcd seems to take approx 1GB of RAM when used as a backing store for a small
  k8s install

```bash
$ etcdctl get name
# retruns nothing

$ etcdctl put name eoin
$ etcdctl get name
name
eoin
# returns the key and then the value separated by newline
```

### Membership Reconfiguration

Adding and removing new nodes can be risky

#### Before etcd 3.4+

Scenario: Add a new node with empty data to a cluster by issuing a `member add`
operation

1. When a new member with empty data joins a cluster it might overwhelm the
   leader by asking it for a lot of data
2. If the leader gets overwhelmed then it might miss sending heartbeats for
   longer than `election-timeout`
3. Which can cause other nodes to do an election

You really don't want to lose a node in a 3 node cluster while you are adding a
new node

Adding a node to a 2 node cluster

1. 2 node cluster, quorum is 2
1. Add a new node via `member add name peer-url-aka-the-2380-one`
1. now the quorum is still 2 but from the cluster pov a network partition has
   happened
1. start the new etcd process (because the cluster is expecting it now). it
   should start pulling data from the leader

Remember that the cluster is not available unless it has a leader i.e. it is not
available during a leadership election

If you lose the leader you cannot revert a `member add`

#### In etcd 3.4+

In etcd 3.4+ you can add member as a "learner" or "non-voting member" (by
passing a learner flag). Once the learner node has caught up you can promote it
via `member promote`

> To replace the machine, follow the instructions for removing the member from
> the cluster, and then add a new member in its place. If the cluster holds more
> than 50MB, it is recommended to migrate the failed member's data directory if
> it is still accessible.

### etcd operator

- has been a bit unmaintained but apparently that is changing

### data layout on disk

- All data is kept in a single directory
- `./default.etcd` in the CWD of the etcd process is the default

```bash
# the main db file, also called the "persisted keyspace"
# this is a boltdb file and can be introspected using bolt db tools e.g. bbolt
<data-dir>/member/db

# multiple .snap snapshots of the WAL
<data-dir>/member/snap/<stuff>-<stuff>.snap

# multiple WAL files
# * writes happen to the WAL file first
# * etcd considers a write successful if it was written to the WAL successfully
#   because it can replay the WAL to add it to the boltdb file later if necessary
<data-dir>/member/wal/<stuff>-<stuff>.wal
```

When a write operation comes in:

1. the operation is appended to the WAL
    - at this point the write is considered permenant
2. The write is immediately copied to the _persisted keyspace_ (main `db` file)
   which is used for reads

To stop the WAL file from growing forever

1. etcd takes a snapshot every `--snapshot-count` writes
    1. create a snapshot file
    2. record the revision that the snapshot was created at to the WAL
    3. Remove wal files (TODO: files or file entries?) older than the snapshot

If it needs to recover it finds the newest snapshot and then replays the WAL log
entries **after** that snapshot - this greatly speeds up recovery.

RAFT ensures the WAL log is the same on all members of the etc cluster.

QUESTION: are the many .snap files required or is only the newest one used?
QUESTION: why are there multiple .wal files? there seems to be as many snap
files are there are wal files - coincidence???

```bash
./etcd \
  --listen-client-urls=http://$PRIVATE_IP:2379 \
  --advertise-client-urls=http://$PRIVATE_IP:2379
```

You can configure an etcd cluster either by static member information or by
dynamic discovery.

### Tools

- etcdctl
    - general admin tool for etcd
    - comes with etcd
- etcd-dump-db
    - dumps the db file
- etcd-dump-logs
    - dumps the WAL
    - https://github.com/etcd-io/etcd/tree/master/tools/etcd-dump-logs
- auger
    - https://github.com/jpbetz/auger
    - lets you work with protobuf encoded values of the kind k8s creates in etcd
    - you can pipe `etcdctl` output into it to decode
- bbolt
    - introspects bolt (the `db` file used by etcd is a bolt db)
    - https://github.com/boltdb/boltd

```bash
# see all keys in the database
$ ETCDCTL_API=3 etcdctl get --prefix '' --keys-only

# check etcd running via kubectl
$ kubectl get componentstatuses


$ docker ps | grep etcd

# check health of the **cluster**
# checks that the cluster has a leader and has quorum
# it actually tries to do a "quorum write" to verify
$ curl -L http://127.0.0.1:2379/health

# this is a weaker check than the one above
# this queries the indivisual node
# it does not check that the cluster has a leader and has quorum
$ ETCDCTL_API=3 etcdctl --write-out=table endpoint status

# check members
# good for checking what cluster members this node can see
$ ETCDCTL_API=3 etcdctl --write-out=table member list
```

### Interpreting log files

Logs have a capital letter after the timestamp

- `I` indicates informational
- `W` indicates warning
- `E` indicates error
- `C` indicates a server crash

In k8s the **only** component which talks directly to etcd is `kube-apiserver`
so you can find out how k8s uses etcd by looking at the `kube-apiserver` log
file

Taking backups before you do etcd upgrades or k8s upgrades or system upgrades is
sensible
