# Podcast with Heidi Howard (Research fellow in Comp Sci at Cambridge)

Sources

- Raft
    - https://raft.github.io/raft.pdf
    - good first paper to introduce this area
- Paxos made moderately complex
    - Good 2nd paper after the Raft paper
- _The part-time parliment_ by Lamport is the original paper which pioneered
  distributed systems
    - https://lamport.azurewebsites.net/pubs/lamport-paxos.pdf
    - fairly vague so it's hard to be sure whether things (e.g. Raft) are or are
      not Paxos
- _Paxos made simple_ by Lamport
    - https://lamport.azurewebsites.net/pubs/paxos-simple.pdf
    - Lamports attempt to make paxos more obvious

Distributed consensus algorithms

Distributed system = 1. concurrent: multiple things happening at the same
time 1. asynchrony: we don't know how long messages will take to get to their
destinatin 1. failures: we don't know _if_ they will get to their destination

Consensus = making a decision in a distributed system examples: deciding a vaule
in a register deciding an ordering of operations to be applied to a database

Consensus has 3 components:

1. the value you agree is the value somebody proposed (non triviality component)
1. when you decide a value that is final then everybody finds ot the same vaule
   (the safety component)
1. if everything is working ok in the ystem the system will reach a decision
   (the progress component)

Luckily we don't always need consensus in a distributed systemd because it is
quite expensive in terms of number of messages exchanged

YOu need consensus when you need _strong consistency guarantees_ e.g. in a
key-value store where you absolutely have to have linearizability, you need
consensus when you can't rely on the clocks being synchronized

e.g. Google spanner avoids the need for consensus by putting atomic clocks in
their data centers if you can rely on timestamps to be consistent within a very
small margin then you don't need consensus

- Consesus is the method of achieveing very strong consistency
- in terms of CAP you are getting strong C but compromising availablity A i.e.
  it is a system focusing on C_P
    - many consensus systems rely on majorities - if a majority of nodes goes
      down then it can't reach consensus
    - e.g. 3 node system can tolerate 1 failure
    - 5 node system can tolerate 2 failures
    - 7 node system can tolerate 3 failures
    - people don't tend to scale the system beyond 7 nodes because the latency
      and number of messages you need to exchange to get consesus goes up a lot
      as you add notes - a conesnsus system is the opposite of a "scalable
      system" - as you add more nodes it gets slower!
    - examples
        - etcd recommends no more than 7 nodes
          https://cwiki.apache.org/confluence/display/ZOOKEEPER/Zab+vs.+Paxos
        - Google Chubby recommends no more than 5 nodes
        - Most systems recommend an _odd_ number of nodes because an odd number
          of N nodes tolerates as much failure as a system with N+1 nodes (the
          next even number) because of the majority thing
- Leadership election = deciding which node gets to make decisions
    - typically the leader backs up all its decisions to the majority of systems
    - nodes vote for the leader (views, ballots, term numbers)
    - nodes can vote multiple times

- Paxos algorithm
    - multi paxos = an elected leader makes multiple decisions ("paxos" and
      "multi paxos" are used synonmysly
    - single degree paxos/vanilla paxos/classis paxos = a new leader is elected
      for each decision
    - paxos has 2 phases
        1. majority of nodes get together to elect a leader
        2. leader chooses a value and backs it up to a majority of nodes
    - systems which use paxos
        - chubby (google)
            - https://ai.google/research/pubs/pub27897
        - cassandra
    - how it works
        1. you send your write request to the leader
        1. leader decides on what order to write things
        1. leader tells other nodes what order to write in
        1. when majority have anssered that they have accepted taht order the
           leader will commit that write and acknowledge to you that the write
           was successful
    - theoritically you should get consensus for reads too but we often skip
      this in practice
- Raft algorithm
    - a good well articulated simplified version of paxos specifically designed
      for state machine replication (which is the most common use-case for
      paxos)
    - https://raft.github.io/raft.pdf
    - It is a good starting point for undertanding the basics of Paxos
    - used by
        - etcd
        - kubernetes
- ZAB (Zookeeper atomic broadcast) algorithm
    - very similar to multi-paxos
    - https://cwiki.apache.org/confluence/display/ZOOKEEPER/Zab+vs.+Paxos
    - Used in
        - Zookeeper
