# Cloud-init Notes

An advantage of coud-init yaml is that it can do a box restart if required and
then continue iwht the provisioning

## How often does cloud-init run?

Each module can be annotated to run

- once,
- once per-instance,
- always.

```yaml
cloud_final_modules:
    - [scripts-user, always]
```

## Waiting on cloud init from script

> For scripts external to cloud-init looking to wait until cloud-init finished,
> the cloud-init status subcommand can help block external scripts until
> cloud-init is done without having to write your own systemd units dependency
> chains => i can tell when cloud init finishes in a script

    cloud-init status --wait

At launch AWS runs

1. User data scripts
1. Cloud init directives

- You can configure it to run those at restart too - is that useful for us?
    - https://aws.amazon.com/premiumsupport/knowledge-center/execute-user-data-ec2/

Logs go to `/var/log/cloud-init-output.log`

- When a user data script is processed, it is copied to and executed from
  /var/lib/cloud/instances/instance-id/.
- The script is not deleted after it is run.
    - Be sure to delete the user data scripts from
      /var/lib/cloud/instances/instance-id/ before you create an AMI from the
      instance. Otherwise, the script will exist in this directory on any
      instance launched from the AMI.

The "user data" field can contain either

1. A shell script
2. Cloud init directives

The instance uses the first line to decide which kind of script it has

- A `#cloud-config` line at the top identifies this user data as cloud init
  directives
    - it must be valid YAML
- A `#!/some/path` line at the top identifies this user data as shell script

```
#cloud-config
repo_update: true
repo_upgrade: all

packages:
 - httpd
 - mariadb-server

runcmd:
 - [ sh, -c, "amazon-linux-extras install -y lamp-mariadb10.2-php7.2 php7.2" ]
 - systemctl start httpd
 - sudo systemctl enable httpd
 - [ sh, -c, "usermod -a -G apache ec2-user" ]
 - [ sh, -c, "chown -R ec2-user:apache /var/www" ]
 - chmod 2775 /var/www
 - [ find, /var/www, -type, d, -exec, chmod, 2775, {}, \; ]
 - [ find, /var/www, -type, f, -exec, chmod, 0664, {}, \; ]
 - [ sh, -c, 'echo "<?php phpinfo(); ?>" > /var/www/html/phpinfo.php' ]

# this line tells cloud-config to send the output of runcmd to the log too (handy for debugging)
output : { all : '| tee -a /var/log/cloud-init-output.log' }
```

Cloud init

- Config: `/etc/cloud/cloud.cfg`
- Logs `/var/log/cloud-init-output.log`

```
cloud-init features
status
status --long

# grabs a bunch of relevant logs and writes them to ~/cloud-init.tar.gz
collect-logs


# it keeps a bunch of metadata in /run/cloud-init/instance-data.json and you can query it
cloud-init query --list-keys
```
