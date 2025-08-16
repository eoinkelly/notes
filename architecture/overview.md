# Message brokers

Quotes from: "How to split a Monolith":

> Distributed systems are messy because of how the pieces interact over time,
> rather than which pieces are interacting.

> The complexity of a system lies in its protocol not its topology, and a
> protocol is what you create when you cut your monolith into pieces.

> A message broker is an architectural pattern for message validation,
> transformation and routing. It mediates communication amongst applications,
> minimizing the mutual awareness that applications should have of each other in
> order to be able to exchange messages, effectively implementing decoupling.

Patterns

- hub and spoke
- message bus
    - Enterprise service bus
- publish subscribe pattern
- examples
    - Redis
    - Kafka
    - RabbitMQ
    - ActiveMQ
- features
    - multiple receivers,
    - reliable storage,
    - guaranteed message delivery
    - sometimes transaction management

Guidelines

- Use verbs instead of nouns to name your services to minimize cross-service
  requests
- Don't do a big bang rewrite
- The logical boundary of a service and the deployment boundary of a service
  might be different!
    - your customer service has code that knows how to query your customer DB
      but that code might be deployed as part other services for them to use
    - see "engines pattern"
    - each team creates a gem/package which implements access to their service's
      data store
        - they use this gem themselves
        - other teams can use the gem to get access to the data center
        - the gem is loaded as an "engine" in the other apps
        - pros/cons (top of my head, no research yet)
            - ++ no cross-service http requests and the associated latency and
              having to build and version APIs
            - ?? what needs to make happens to change the public API of the gem
            - -- you can make changes under the hood in your gem/DB but you need
              to support old versions as well as the latest
            - -- no event queue needed
