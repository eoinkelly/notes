# Elastic Load Balancer

* distributes incoming application traffic across
    * Amazon EC2 instances
    * containers
    * IP addresses
* does health checks to ensure targets (which can be EC2 instances, containers, IP addresses) are healthy
* can distribute traffic across multiple AZs
    * QUESTION: can it do across multiple regions?
* has integrated certificate management and SSL decryption
    * lets you offload CPU intensive workloads from your applications
* allows you to monitor your applications and their performance in real time with
    * Amazon CloudWatch metrics,
    * logging,
    * request tracing
* can load balance across AWS and on-premises resources using the same load balancer.
* three types available:
    1. Application load balancer
    1. Network load balancer
    1. Classic load balancer
