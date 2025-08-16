# Vagrant

Quick start

```
cd /somewhere/you/want/to/share/with/vm
vagrant init ubuntu/trusty64

# do stuff on server
vagrant ssh
```

Vagrant shares a directory at /vagrant with the directory on the host containing
your Vagrantfile,

## Automatic provisioning

via a shell script that you supply a path relative to the shared directory

```
Vagrant.configure("2") do |config|

  # name of the box to use
  config.vm.box = "hashicorp/precise32"

  # the relative path to the provisioning script
  config.vm.provision :shell, path: "bootstrap.sh"


  # Forward host:4567 to guest:80
  config.vm.network :forwarded_port, host: 4567, guest: 80
end
```

```
vagrant up                          # will run provisioning
vagrant reload --provision          # restart VM and re-run provisioning script
```

Vagrant can

1. suspend
    ```
    vagrant suspend # suspends
    vagrant up      # restarts it
    ```
2. halt
    ```
    vagrant halt    # shuts it down
    vagrant up      # reboots it
    ```
3. destroy
    ```
    vagrant destroy # stop VM, power it down, delete all hard disks
    vagrant up      # rebuild it from scratch
    ```

the VM

## Providers

Providers are vagrants name for "things that provide virtual machines" e.g.

- Virtualbox
- Vmware
- AWS

Vagrant uses virtualbox by default but has plugins for others
