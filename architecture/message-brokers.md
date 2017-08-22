# Message brokers

Quotes from: "How to split a Monolith":

> Distributed systems are messy because of how the pieces interact over
> time, rather than which pieces are interacting.

> The complexity of a system lies in its protocol not its topology, and a
> protocol is what you create when you cut your monolith into pieces.

> A message broker is an architectural pattern for message validation,
> transformation and routing. It mediates communication amongst
> applications, minimizing the mutual awareness that applications should have
> of each other in order to be able to exchange messages, effectively
> implementing decoupling.

Patterns

* hub and spoke
* message bus
    * Enterprise service bus
* publish subscribe pattern
* examples
    * Redis
    * Kafka
    * RabbitMQ
    * ActiveMQ
* features
    * multiple receivers,
    * reliable storage,
    * guaranteed message delivery
    * sometimes transaction management
