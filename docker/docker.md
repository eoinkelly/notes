
Is a replacement for virtual machines

++ VM host OS takes 10-15% of box resources, docker is much lighter
++ much faster startup times
++ it has a "dev workflow"

the "docker images" are immutable and are built up in layers a bit like git repo
you put the mutable things (like logging, app code) on dirs "mounted" in the docker image
    e.g. apache docker image has access to `/var/www` which is mutable and also `/var/log` for its logs

How does docker relate to chef?
    chef automates the setup of VMs
    docker image is a specialized VM designed to run just one app e.g. nginx
    so you would probably still want something to automate setting up tht little VM e.g. chef/ansible/puppet

    it does mean chef doesn't need to run on the deployed docker image
        because the image is immutable so you would just push a new version not mutate it in place
        ??? does that mean you would not have chef-client running inside the docker image?

docker images can use ubuntu but often use a much smaller distro (busybox) that just provides enough to run a single app
    ++ this makes the images much smaller and quicker to work with
