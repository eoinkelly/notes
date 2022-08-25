

# Cheatsheet of ccommands
```
brew install awscli aws-elasticbeanstalk

eb init
eb list
eb ssh ENVNAME
```

## Environment

When you update the environment variables for the instances on the Configure page of EB, it's not immediately avilable in the shell

> environment properties are passed only to the application and can't be viewed by connecting an instance in your environment and running env

?? how are they put in to the instance?
?? will rails automatically pick them up?

```bash
# you can do this to get the env var from a script
$ /opt/elasticbeanstalk/bin/get-config environment -k MY_ENV_VAR
```

## Overview

* designed to run and manage **web** apps - it is quite tuned for this use-case (see the web server tier and worker tier)
* no additional charge beyond what you use in infrastructure
* looks like it can do blue-green (rolling) deploys

Terminology

* Application
    * a logical collection of EB components
        * environments
        * application versions
        * environment configurations
* Application version
    * a specific labelled iteration of deployable code
    * points to an S3 object which contains the deployable code
    * an application has many versions
    * versions are 1) uploaded and 2) deployed.
    * you can deploy any version you have uploaded
* Environment
    * a collection of AWS resources running an _application version_
    * an environment runs exactly one _application version_ at a time
    * an _application version_ can be running in multiple enviornments at the same time
* Enviornment tier
    * each environment is within an "environment teir"
    * the tier decides which resources an application running in an enviornment has access to
    * there seem to be only two environment tiers:
        1. Web server environment tier
            * includes ELB, ASG, 1+ EC2 instances
            * Every environment has a CNAME which points to a load balancer as `ENV_NAME.REGION.elasticbeanstalk.com`
        1. Worker environment tier
            * includes IAM role, ASG, 1+ EC2 instances, SQS queue
                * NB: workers always get an SQS queue!!!
    * Q: can you make your own environment tier?
* Environment configuration
    * set of parameters and settings which define how an enviornment behaves
    * when you update an environment configuration, EB automatically applies the changes to the resources
    * you can have template configurations that you use as starting points to create _environment configurations_ - these are called _saved configurations_
* Platform
    * A platform is a combination of an operating system, programming language runtime, web server, application server, and Elastic Beanstalk components.
    * examples
        * Amazon Linux + Ruby 2.6.3 + Puma + RDS + etc. etc.







platforms have versions which are either supported or retired
* seem to be called _solution stacks_ sometimes
* Most run on Amazon Linux
* retired platform versions are only available 90 days after the retirement date

Stacks as of 2019-10-05:

```
"64bit Amazon Linux 2018.03 v4.10.2 running Node.js",
"64bit Amazon Linux 2018.03 v2.8.15 running PHP 5.4",
"64bit Amazon Linux 2018.03 v2.8.15 running PHP 5.5",
"64bit Amazon Linux 2018.03 v2.8.15 running PHP 5.6",
"64bit Amazon Linux 2018.03 v2.8.15 running PHP 7.0",
"64bit Amazon Linux 2018.03 v2.8.15 running PHP 7.1",
"64bit Amazon Linux 2018.03 v2.8.15 running PHP 7.2",
"64bit Amazon Linux 2018.03 v2.9.2 running Python 3.6",
"64bit Amazon Linux 2018.03 v2.9.2 running Python 3.4",
"64bit Amazon Linux 2018.03 v2.9.2 running Python",
"64bit Amazon Linux 2018.03 v2.9.2 running Python 2.7",
"64bit Amazon Linux 2018.03 v2.10.2 running Ruby 2.5 (Passenger Standalone)",
"64bit Amazon Linux 2018.03 v2.10.2 running Ruby 2.4 (Passenger Standalone)",
"64bit Amazon Linux 2018.03 v2.10.2 running Ruby 2.3 (Passenger Standalone)",
"64bit Amazon Linux 2018.03 v2.10.2 running Ruby 2.2 (Passenger Standalone)",
"64bit Amazon Linux 2018.03 v2.10.2 running Ruby 2.1 (Passenger Standalone)",
"64bit Amazon Linux 2018.03 v2.10.2 running Ruby 2.0 (Passenger Standalone)",
"64bit Amazon Linux 2018.03 v2.10.2 running Ruby 1.9.3",
"64bit Amazon Linux 2018.03 v2.10.2 running Ruby 2.5 (Puma)",
"64bit Amazon Linux 2018.03 v2.10.2 running Ruby 2.4 (Puma)",
"64bit Amazon Linux 2018.03 v2.10.2 running Ruby 2.3 (Puma)",
"64bit Amazon Linux 2018.03 v2.10.2 running Ruby 2.2 (Puma)",
"64bit Amazon Linux 2018.03 v2.10.2 running Ruby 2.1 (Puma)",
"64bit Amazon Linux 2018.03 v2.10.2 running Ruby 2.0 (Puma)",
"64bit Amazon Linux 2018.03 v2.10.1 running Ruby 2.6 (Passenger Standalone)",
"64bit Amazon Linux 2018.03 v2.10.1 running Ruby 2.6 (Puma)",
"64bit Amazon Linux 2018.03 v3.2.2 running Tomcat 8.5 Java 8",
"64bit Amazon Linux 2018.03 v3.2.2 running Tomcat 8 Java 8",
"64bit Amazon Linux 2018.03 v3.2.2 running Tomcat 7 Java 7",
"64bit Amazon Linux 2018.03 v3.2.2 running Tomcat 7 Java 6",
"64bit Windows Server Core 2016 v2.2.2 running IIS 10.0",
"64bit Windows Server 2016 v2.2.2 running IIS 10.0",
"64bit Windows Server Core 2012 R2 v2.2.2 running IIS 8.5",
"64bit Windows Server 2012 R2 v2.2.2 running IIS 8.5",
"64bit Windows Server Core 2016 v1.2.0 running IIS 10.0",
"64bit Windows Server 2016 v1.2.0 running IIS 10.0",
"64bit Windows Server Core 2012 R2 v1.2.0 running IIS 8.5",
"64bit Windows Server 2012 R2 v1.2.0 running IIS 8.5",
"64bit Windows Server 2012 v1.2.0 running IIS 8",
"64bit Windows Server 2008 R2 v1.2.0 running IIS 7.5",
"64bit Windows Server Core 2012 R2 running IIS 8.5",
"64bit Windows Server 2012 R2 running IIS 8.5",
"64bit Windows Server 2012 running IIS 8",
"64bit Windows Server 2008 R2 running IIS 7.5",
"64bit Amazon Linux 2018.03 v2.12.17 running Docker 18.06.1-ce",
"64bit Amazon Linux 2018.03 v2.16.0 running Multi-container Docker 18.06.1-ce (Generic)",
"64bit Debian jessie v2.12.17 running Go 1.4 (Preconfigured - Docker)",
"64bit Debian jessie v2.12.17 running Go 1.3 (Preconfigured - Docker)",
"64bit Debian jessie v2.12.17 running Python 3.4 (Preconfigured - Docker)",
"64bit Amazon Linux 2018.03 v2.9.2 running Java 8",
"64bit Amazon Linux 2018.03 v2.9.2 running Java 7",
"64bit Amazon Linux 2018.03 v2.13.0 running Go 1.13",
"64bit Amazon Linux 2018.03 v2.6.15 running Packer 1.0.3",
"64bit Amazon Linux 2018.03 v2.12.17 running GlassFish 5.0 Java 8 (Preconfigured - Docker)"
```


* Platforms
    * Single container docker
        * includes nginx as a proxy in front of the container
        * described in a Dockerfile or Dockerrun.aws.json definition
        * `eb` command provides somewhat of a wrapper around the `docker` command locall
    * Multicontainer docker
        * Uses ECS to do deployments under the hood!
    * Ruby
        * includes puma


Managed updates

>  automatically applies patch and minor updates to the operating system (OS),
>  runtime, web server, application server, and Elastic Beanstalk components
>  for an Elastic Beanstalk supported platform version. You can configure
>  managed updates to apply only patch updates, or minor and patch updates. You
>  can also disable managed updates completely.

* these updates apply to the **platform** not your code


YOu can create custom AMI images to use in the EB platform

* You can create a whole custom platform using Packer
    * you provide a packer template
    What is the advantage of this?

An Elastic Beanstalk platform comprises
1. an AMI configured to run a set of software that supports an application,
1. metadata that can include custom configuration options and default configuration option settings.
