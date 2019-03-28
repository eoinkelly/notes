# Well Architected Framework

* A set of best practices for building on the AWS cloud
* Created by AWS solutions architects (employees)


5 Pillars

1. Operational excellence
    * run and monitor systems to deliver business value
    * continually improve supporting processes
1. Security
    * protect information, systems, assets
    * risk assessments
    * risk mitigations
1. Reliability
    * ability to recover from infrastructure disruptions
    * dynamically aquire computing resources to meet demand
    * mitigate disruptions such as misconfiguration or network latency
1. Performance efficiency
    * use computing resources effeciently
    * maintain that effeciency as demand changes and technologies evolve
1. Cost optimization
    * deliver business value at the lowest price point

When architecting, you make trade-offs between these pillars depending on business context e.g.
* Trade off reliability for cost optimization in development environments
* Security and operational excellence are generally not traded off against

Terms

* Component
    * the code, configuration and AWS resources which deliver a feature
    * often owned by one entity (person/team) within the business
* Workload
    * a collection of components working together to deliver business value
    * is usually the level of detail that business & technology leaders discuss
* Milestones
    * mark key phases in your architecture
    * design, testing, go-live, production
* Architecture
    * How components work together in a workload
* Technology portfolio
    * the collection of workloads which are required for the business to operate

Traditional, centralised, architecture teams

* Technical architect
    * designs infrastructure
* Solutions architect
    * designs software
* Data architect
* Networking architect
* Security architect

These teams often use

* TOGAF http://pubs.opengroup.org/architecture/togaf9-doc/arch/
* Zachman Framework https://www.zachman.com/about-the-zachman-framework

AWS distributes architecture across many teams rather than one central team of architects:

They mitigate the risks by

1. Practices
    * enable each team to have the capability to make decisions
    * have experts in place to help
        * Question: where do experts sit? in team or outside?
    * give teams access to a virtual community of principal engineers who review their designs and help them follow best practices
    * principal engineering community works to make the best practices available and visible e.g. lunch-time talks are recorded and used as training materials
1. Mechanisms
    * automated checks for compliance to best practices to make sure standards are met

UP TO GENERAL DESIGN PRINCIPLES
