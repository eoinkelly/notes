# Distributed Systems lecture series

https://www.youtube.com/playlist?list=PLeKd45zvjcDFUEv_ohr_HdUFe97RItdiB

My notes on the lecture notes at
https://www.cl.cam.ac.uk/teaching/2021/ConcDisSys/dist-sys-notes.pdf

- Unlike threads running in a process, we don't have shared memory in a
  distributed system
    - Instead we rely on message passing

Potential problems in a distributed system

1. Communication failure
2. Node may crash

- Fault tolerance
    - a system which can continue to function even if some parts are faulty
    - Real systems are only fault tolerant up to a certain number/type of faults

Bandwidth = the volume of data that can be transferred per unit time Latency =
the delay from the time a message is sent until it is received

Once you zoom out a little bit, REST is basically a kind of RPC

## Two generals problem

- two encamped armies, far apart, who must communicate though area controlled by
  the city, try to agree on a plan to attack the city
- if both armies attack at same time they will succeed, otherwise they will fail
- failure cannot be undone
- describes a system with an unreliable communication channel (sender doesn't
  know if it was delivered, reciever doesn't know if anything was sent or not)
- what protocol should the generals use to handle this

The generals can build up confidence by exchanging messages repeatedly

It can be proved that the generals cannot reach certainty about the plan by
exchanging messages

i.e. it is impossible for one node to be certain about the state of another node
