# Amazon Linux

https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/amazon-linux-ami-basics.html

* is CentOS based
* uses yum for packages
* will install security updates which are rated **critical** or **important** on launch
    * there may be other security updates available
* pre-installs the AWS CLI tools
* the default user is `ec2-user`
* There are normal and minimal versions of the AMI available
* Updates
    * Updates are provided via a pre-configured yum repository hosted in each Amazon EC2 region.
    * Security updates are automatically applied on the initial boot of the AMI.
    * Upon login, the Message of the Day (/etc/motd) indicates whether or not any additional updates are available.
* The Amazon Linux AMI is built and maintained as a rolling release.
    * They encourage users to think of the Amazon Linux AMI as a single river of packages, of which the AMI images themselves are just snapshots in time

Useful files

```
# identifies the image
/etc/image-id

# specify currently installed release
/etc/system-release

# machine readable version of system-release
/etc/system-release

# OS release details
/etc/os-release
```

Only "critical" and "important" security upgrades are installed on launch. This is controlled by

```
#cloud-config
repo_upgrade: security
```

TODO this doesn't seem to work properly - what am i missing?


Logs live in `/var/log/ecs`

```bash
# Add -y to default to 'yes' answers to interactive prompts
yum update -y

# To list all updates that are security relevant, and get a return code on whether there are security updates use:
yum --security check-update

# To upgrade packages that have security errata (upgrades to the latest available package) use:
yum --security update

# To upgrade packages that have security errata (upgrades to the last security errata package) use:
yum --security update-minimal
```

> designed to be used with online package repositories hosted in each Amazon
> EC2 region. These repositories provide ongoing updates to packages in Amazon
> Linux 2 and the Amazon Linux AMI, as well as access to hundreds of additional
> common open-source server applications. The repositories are available in all
> regions and are accessed using yum update tools. Hosting repositories in each
> region enables us to deploy updates quickly and without any data transfer
> charges.
>
> Your instance must have access to the internet in order to access the repository.
>
> Amazon Linux 2 and the Amazon Linux AMI are updated regularly with security
> and feature enhancements. If you do not need to preserve data or
> customizations for your instances, you can simply launch new instances using
> the current AMI. If you need to preserve data or customizations for your
> instances, you can maintain those instances through the Amazon Linux package
> repositories. These repositories contain all the updated packages. You can
> choose to apply these updates to your running instances. Older versions of
> the AMI and update packages continue to be available for use, even as new
> versions are released.
>
> Your instance must have access to the internet in order to access the repository.
>
> Amazon Linux is configured to download and install security updates at launch
> time. This is controlled using the following cloud-init setting:
> repo_upgrade.
