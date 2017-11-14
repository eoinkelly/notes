# Autoscaling

* launches _and_ terminates resources based on some criteria
* as well as scaling to meet demand, it can be used to automatically detect unhealthy instances and replace them
* it can send to EC2 instances in multiple zones

You can setup autoscaling based on

1. application load e.g. CPU utilization at 50% (target tracking)
1. request count per target

Other than EC2 instances, you can also setup automatic scaling of

* ECS
* DynamoDB
* EC2 spot fleets
* (a few other things we don't use)

at the application level.


It does not seem to require the use of an ELB
running instances can be attached to an autoscaling group

Launch configurations

* have mostly the same set of options as when launching an instance manually e.g.
    * block storage
    * which security group
    * associate a key pair
* Whenever an Auto Scaling group launches a new instance, it uses the currently associated launch configuration as a template for the launch.
* AWS can derive a launch configuration from a running instance or you can manually create your own
* Launch configurations are immutable - if you want to make changes, you must make a new one and replace the old one in the autoscaling group
* control whether new instances get a public IP or not

## Autoscaling group

* requires an existing launch configuration
    * will prompt you to create a launch configuration
* requires you to specify which subnets (note can be plural) you are setting up autoscaling across
* QUESTION: can autoscaling cross VPCs?

Lifecycle hooks
* you can hook into the creation and termination of an instance to do things
* examples
    * use a terminate hook to preserve your fleet’s log files by copying them to an Amazon S3 bucket when instances go out of service.

Fleet size

* you set a minimum, desired, maximum
* autoscaling tries to keep your fleet size at desired
* it will automatically start new instances to keep them balanced across whatever AZs you have configured

Types of scaling

* Scheduled scaling
    * You can adjust the size of the fleet on a schedule
* Dynamic scaling
    * Adjusts the fleet size based on cloudwatch metrics
