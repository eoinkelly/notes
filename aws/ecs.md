# ECS

Cluster

- a group of EC2 instances
- each instance runs the ECS agent
- clusters contain services

Service

- a collection of containers which achieve a particular goal
- services contain containers defined by one or more tasks

> Service allows you to run and maintain a specified number (the "desired
> count") of instances of a task definition simultaneously in an ECS cluster.

- if any task fils or stops the _ECS service scheduler_ will launch ahter
  instance of **the task** to replace it and maintain the _desired count_.
- can run a service behind a load balancer which will distribute traffic across
  all tasks within the service
- services are what is auto-scaled and load balanced

- services have "task placement" and "constraints" which decide how task
  instances should be allocated to EC2 instances across the cluster

Task

- a blueprint for a service (service is the runtime thing created by
  instantiating tasks)
- kind of like docker-compose.yml
- specify
    - which images to use
    - wha resources your containers can use
    - what ports to map
    - what data volumes to us
    - IAM roles your tasks should use
- tasks are "registered" with ???
- tasks have revisions

Logs

- containers log to Cloudtrail via the awslogs Log Driver

Service Scheduler

> Amazon ECS provides a service scheduler (for long-running tasks and
> applications), the ability to run tasks manually (for batch jobs or single run
> tasks), with Amazon ECS placing tasks on your cluster for you, and the ability
> to run tasks on the container instance that you specify, so that you can
> integrate with custom or third-party schedulers or place a task manually on a
> specific container instance

seems to be their version of kubernetes
