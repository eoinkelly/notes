task-level _ `memory` _ `memory: null` shows as `--` in the _Task Size_ part of
the GUI _ is a hard limit - ECS will kill the task if it crosses this limit _
`cpu` \* `cpu: null` shows as `--` in the _Task Size_ part of the GUI

- contianer definition level
    - `cpu`
        - `cpu: 0` shows up in the summary line of the container definition in
          the GUI
        - maps to _CPU shares_ in docker world
        - is a soft limit on Linux, hard on Windows
        - linux will guarantee a container that it has at least the allocated
          CPU shares available but if the task isn't using them it will give
          them to another task even if that would exceed that tasks allocation
        - minimum valid value is 2.
            - values below 2 (including `null` are interpreted as
                - Agent v 1.1.0 or older:
                    - null, 0 => passed to docker as 0, which docker then
                      converts into 1024 shares
                    - 1 => passed to docker as 1, linux kernel converts to 2
                - Agent v 1.2.0 or later:
                    - null, 0, 1 => passed to docker as 2 shares
                    - Question: does this mean that not passing CPU shares
                      restricts us to a very small allocation if there is
                      contention?
    - `memory`
        - shows up as the _Hard memory limit_ in the GUI
        - corresponds to the `--memory` option to docker - see
          https://docs.docker.com/config/containers/resource_constraints/
        - is a hard limit at which the docker daemon will kill the container
        - this is the number which appears as the memory limit in `docker stats`
          => this number is given to docker to enforce
    - `memoryReservation`
        - shows up as the _Soft memory limit_ in the GUI
        - corresponds to the `--memory-reservation` option to docker - see
          https://docs.docker.com/config/containers/resource_constraints/
        - Docker will use it to decide which container gets priority if there is
          memory contention on an instance
            - it should always be set lower than `memory`
            - soft limit
            - this might be something that the container agent monitors for? or
              maybe it's just a number used in reservation calculations?

Question: does ECS use the hard or soft memory limit when doing placement?

Observation:

The memory bar for a task shows striped if you have set `memory` in the
container but not `memoryReservation` and set nothing at the task level

You can only get metrics per cluster or per service in Cloudwatch - you can't
get them per task or per container

# How to read ECS metrics graphs

Common to all graphs

- All times are in UTC
- The X-axis shows last 24hrs (values averaged over 5 mins)
- The graphs do auto-refresh (not sure what the interval is)

## How to read the Metrics tab of a cluster

How useful these graphs are depends on how well you have specified your
reservationss of CPU and memory at the task and container level.

The page shows 4 graphs each showing a max, min, average

1. CPU Utilization
    - the total CPU units in use by Amazon ECS tasks on the cluster, divided by
      the total CPU units that were registered for all of the container
      instances in the cluster
    - only available for tasks using the EC2 launch type
    - Total CPU units in cluster = total num vCPUs in cluster \* 1024
1. Memory Utilization
    - the total memory in use by Amazon ECS tasks on the cluster, divided by the
      total amount of memory that was registered for all of the container
      instances in the cluster
    - only available for tasks using the EC2 launch type
1. CPU Reservation
    - the total CPU units that are reserved by Amazon ECS tasks on the cluster,
      divided by the total CPU units that were registered for all of the
      container instances in the cluster
    - only available for tasks using the EC2 launch type
1. Memory Reservation
    - the total memory that is reserved by Amazon ECS tasks on the cluster,
      divided by the total amount of memory that was registered for all of the
      container instances in the cluster.
    - only available for tasks using the EC2 launch type

## How to read the metrics tab of a service

1. CPU Utilization (percentage)
    - the total CPU units in use by the tasks that belong to the service,
      divided by the total number of CPU units that are reserved for the tasks
      that belong to the service.
    - available for both Fargate and EC2 launch types
1. Memory Utilization (percentage)
    - the total memory in use by the tasks that belong to the service, divided
      by the total memory that is reserved for the tasks that belong to the
      service.
    - available for both Fargate and EC2 launch types

Questions

- Does each container have limits set which are enough to not starve it?

You could have no limits on any container but that would make sensible placement
impossible
