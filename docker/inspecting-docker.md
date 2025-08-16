# Inspecting docker with standard tools

Questions I have

1. how to get the full size on disk of a docker image (including all ancestor
   layers)?
    - `docker image ls` or `docker images` will show full size
    - `docker image history IMAGE_ID_OR_TAG` or `docker history IMAGE_ID_OR_TAG`
      will show each layer size if you need it
1. how to see the memory usage of a docker container
    - Use `docker stats`
    - Linux Containers rely on control groups which not only track groups of
      processes, but also expose metrics about CPU, memory, and block I/O usage.
    - stats shows `PIDS` which is the number of pids **within** the container
1. what processes run on the host to run a container.
    - it seems to be that for each running container we have:
        - 1 x containerd-shim for each container
        - 1 x docker-proxy process for each port binding to host
        - 1 x `docker run` command
1. how does `docker exec` work? if PID 1 is not a shell how do we add more
   processes?

```
# processes running on rhel7 with docker-latest and no containers running
root      1204  0.0  0.3 793276 31056 ?        Ssl  Aug10   9:25 /usr/bin/dockerd-latest --add-runtime docker-runc=/usr/libexec/docker/docker-runc --default-runtime=docker-runc --authorization-plugin=rhel-push-plugin --exec-opt native.cgroupdriver=systemd -g /var/lib/docker-latest --selinux-enabled --log-driver=journald -H unix:///var/run/docker.sock -H tcp://127.0.0.1:4243 --add-registry registry.access.redhat.com
root      1508  0.0  0.0 282048  8232 ?        Ssl  Aug10   0:36 docker-containerd -l unix:///var/run/docker/libcontainerd/docker-containerd.sock --shim docker-containerd-shim --metrics-interval=0 --start-timeout 2m --state-dir /var/run/docker/libcontainerd/containerd --runtime docker-runc --runtime-args --systemd-cgroup=true
root      1669  0.0  0.0 201660  7304 ?        Ssl  Aug10   1:35 /usr/libexec/docker/rhel-push-plugin

/usr/bin/dockerd-latest
  --add-runtime docker-runc=/usr/libexec/docker/docker-runc
  --default-runtime=docker-runc
  --authorization-plugin=rhel-push-plugin
  --exec-opt native.cgroupdriver=systemd
  -g /var/lib/docker-latest
  --selinux-enabled
  --log-driver=journald
  -H unix:///var/run/docker.sock
  -H tcp://127.0.0.1:4243
  --add-registry registry.access.redhat.com
docker-containerd
  -l unix:///var/run/docker/libcontainerd/docker-containerd.sock
  --shim docker-containerd-shim
  --metrics-interval=0
  --start-timeout 2m
  --state-dir /var/run/docker/libcontainerd/containerd
  --runtime docker-runc
  --runtime-args
  --systemd-cgroup=true
/usr/libexec/docker/rhel-push-plugin
```

```
# processes running on rhel7 with docker-latest and one **backgrounded** memcached container running

root      1204  0.0  0.5 896304 54132 ?        Ssl  Aug10   9:29 /usr/bin/dockerd-latest --add-runtime docker-runc=/usr/libexec/docker/docker-runc --default-runtime=docker-runc --authorization-plugin=rhel-push-plugin --exec-opt native.cgroupdriver=systemd -g /var/lib/docker-latest --selinux-enabled --log-driver=journald -H unix:///var/run/docker.sock -H tcp://127.0.0.1:4243 --add-registry registry.access.redhat.com
root      1508  0.0  0.0 358244  9220 ?        Ssl  Aug10   0:36 docker-containerd -l unix:///var/run/docker/libcontainerd/docker-containerd.sock --shim docker-containerd-shim --metrics-interval=0 --start-timeout 2m --state-dir /var/run/docker/libcontainerd/containerd --runtime docker-runc --runtime-args --systemd-cgroup=true
root      1669  0.0  0.0 201660  7464 ?        Ssl  Aug10   1:35 /usr/libexec/docker/rhel-push-plugin
root      2979  0.0  0.0 341240  6232 ?        Sl   09:07   0:00 docker-containerd-shim 5a60b5b7162c3d2555543a25c8e6ed7896e668107f582b4991900c11ba983e47 /var/run/docker/libcontainerd/5a60b5b7162c3d2555543a25c8e6ed7896e668107f582b4991900c11ba983e47 /usr/libexec/docker/docker-runc

/usr/bin/dockerd-latest
  --add-runtime docker-runc=/usr/libexec/docker/docker-runc
  --default-runtime=docker-runc
  --authorization-plugin=rhel-push-plugin
  --exec-opt native.cgroupdriver=systemd
  -g /var/lib/docker-latest
  --selinux-enabled
  --log-driver=journald
  -H unix:///var/run/docker.sock
  -H tcp://127.0.0.1:4243
  --add-registry registry.access.redhat.com
docker-containerd
  -l unix:///var/run/docker/libcontainerd/docker-containerd.sock
  --shim docker-containerd-shim
  --metrics-interval=0
  --start-timeout 2m
  --state-dir /var/run/docker/libcontainerd/containerd
  --runtime docker-runc
  --runtime-args
  --systemd-cgroup=true
/usr/libexec/docker/rhel-push-plugin
docker-containerd-shim
  5a60b5b7162c3d2555543a25c8e6ed7896e668107f582b4991900c11ba983e47
  /var/run/docker/libcontainerd/5a60b5b7162c3d2555543a25c8e6ed7896e668107f582b4991900c11ba983e47
  /usr/libexec/docker/docker-runc
```

so there is one extra process running:

```
docker-containerd-shim
  5a60b5b7162c3d2555543a25c8e6ed7896e668107f582b4991900c11ba983e47
  /var/run/docker/libcontainerd/5a60b5b7162c3d2555543a25c8e6ed7896e668107f582b4991900c11ba983e47
  /usr/libexec/docker/docker-runc
```

which corresponds to

```
docker ps
CONTAINER ID        IMAGE                 COMMAND                  CREATED             STATUS              PORTS               NAMES
5a60b5b7162c        docker.io/memcached   "docker-entrypoint.sh"   3 minutes ago       Up 3 minutes        11211/tcp           eointest
```

but if there are many container running via systemd we see

```
docker ps
CONTAINER ID        IMAGE                                                 COMMAND                  CREATED             STATUS                    PORTS                      NAMES
a43d3896a9f5        nzxsed/matrix-sentinel:develop                        "./docker_start.sh"      16 hours ago        Up 16 hours               0.0.0.0:6000->7000/tcp     new-hope-search-app
83a0f40af95b        docker.elastic.co/elasticsearch/elasticsearch:5.5.0   "/bin/bash bin/es-doc"   16 hours ago        Up 16 hours               9200/tcp, 9300/tcp         new-hope-search-elasticsearch
0fd1f66e854b        redis:3                                               "docker-entrypoint.sh"   16 hours ago        Up 16 hours               6379/tcp                   new-hope-search-redis
7b4d4317d894        nzxsed/a-new-hope:develop                             "foreman start"          16 hours ago        Up 16 hours (unhealthy)   0.0.0.0:7999->7000/tcp     new-hope-2
cbd2afc36c99        nzxsed/a-new-hope:develop                             "foreman start"          16 hours ago        Up 16 hours (healthy)     0.0.0.0:7000->7000/tcp     new-hope-1
4194cd13fc4a        nzxsed/egress-service:develop                         "foreman start"          24 hours ago        Up 24 hours (healthy)     0.0.0.0:4000->4000/tcp     egress-service-1
1c6b6c3957a0        docker.io/redis:3.0-alpine                            "docker-entrypoint.sh"   24 hours ago        Up 24 hours               6379/tcp                   redis-erdinger
015eb11c1e76        memcached:1.4-alpine                                  "docker-entrypoint.sh"   24 hours ago        Up 24 hours               0.0.0.0:11211->11211/tcp   new-hope-memcached
```

```
root      1808  0.1  0.0 202716  8664 ?        Ssl  Aug10  28:49 /usr/libexec/docker/rhel-push-plugin
root     12552  0.0  0.0 224436  8140 ?        Ssl  Aug21   0:39 /usr/bin/docker-latest run --rm -e S3_ACCESS_KEY_ID=AKIAJABUQTI7JQTRAXGA -e S3_SECRET_ACCESS_KEY=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 -e AWS_REGION=ap-southeast-2 -e CMS_BUCKET=nzx-prod-c84t3un4 -e ANN_BUCKET=nzx-prod-s7fsd7f98s -e INTERNET_PROXY=http://wnp-int-services.nzx.com:3128 -e SEARCH_URL=http://wlt-sed-nzxcom1.nzx.com:6000 -e RAILS_ENV=uat -e CLAG_URL=http://clag.csu-gxmarket.nzx.com -e CLAG_KEY=ingress -e ISEARCH_URL=isearch.wnc-map.nzx.com -e ISEARCH_USER=sed-services%40nzx.com -e ISEARCH_PASS=w3Y2XyqMqQdw -e SECRET_KEY_BASE=05256d0bfb8bbead806f742b792da8533818524bcfd2147e963d37cddab977042d2441e133fa0ba90c1aaa2b90eeb18a0419482374bc395e099b0f4549faa254 -e RAILS_SERVE_STATIC_FILES=false -e TZ=Pacific/Auckland -e DB_HOST=wnu-postgres1.nzx.com -e DB_PORT=5432 -e DB_NAME=new_hope_uat -e DB_USER=deploy -e DB_PASS=deploy -e CACHE_HOST_01=new-hope-memcached:11211 -e INGRESS_URL=http://wlt-sed-nzxcom1.nzx.com:4000 -e DB_POOL=24 -e WORKERS=2 -e RAM=1024 -p 7000:7000 --link new-hope-memcached --log-driver=journald --name new-hope-1 nzxsed/a-new-hope:develop foreman start
root     12622  0.0  0.0 215984  8072 ?        Ssl  Aug21   0:11 /usr/bin/docker-latest run --rm -e S3_ACCESS_KEY_ID=AKIAJABUQTI7JQTRAXGA -e S3_SECRET_ACCESS_KEY=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 -e AWS_REGION=ap-southeast-2 -e CMS_BUCKET=nzx-prod-c84t3un4 -e ANN_BUCKET=nzx-prod-s7fsd7f98s -e INTERNET_PROXY=http://wnp-int-services.nzx.com:3128 -e SEARCH_URL=http://wlt-sed-nzxcom1.nzx.com:6000 -e RAILS_ENV=uat -e CLAG_URL=https://auth.nzx.com -e CLAG_KEY=44e3901fc4c09d60508f18ee639a5800486b12b1 -e ISEARCH_URL=isearch.wnc-map.nzx.com -e ISEARCH_USER=sed-services%40nzx.com -e ISEARCH_PASS=w3Y2XyqMqQdw -e SECRET_KEY_BASE=05256d0bfb8bbead806f742b792da8533818524bcfd2147e963d37cddab977042d2441e133fa0ba90c1aaa2b90eeb18a0419482374bc395e099b0f4549faa254 -e RAILS_SERVE_STATIC_FILES=true -e TZ=Pacific/Auckland -e DB_HOST=wnu-postgres1.nzx.com -e DB_PORT=5432 -e DB_NAME=new_hope_uat -e DB_USER=deploy -e DB_PASS=deploy -e CACHE_HOST_01=new-hope-memcached:11211 -e INGRESS_URL=http://wlt-sed-nzxcom2.nzx.com:4000 -e DB_POOL=24 -e WORKERS=2 -e RAM=1024 -p 7999:7000 --link new-hope-memcached --log-driver=journald --name new-hope-2 nzxsed/a-new-hope:develop foreman start
root     12695  0.0  0.0 197180  5912 ?        Sl   Aug21   0:00 /usr/bin/docker-proxy -proto tcp -host-ip 0.0.0.0 -host-port 7000 -container-ip 172.17.0.3 -container-port 7000
root     12707  0.0  0.0 275704  4392 ?        Sl   Aug21   0:29 docker-containerd-shim cbd2afc36c99f475ceb7cd304c5c680be28ba380bc2bde21ed182ebf5a162b22 /var/run/docker/libcontainerd/cbd2afc36c99f475ceb7cd304c5c680be28ba380bc2bde21ed182ebf5a162b22 /usr/libexec/docker/docker-runc
root     12784  0.0  0.0 109104  1944 ?        Sl   Aug21   0:00 /usr/bin/docker-proxy -proto tcp -host-ip 0.0.0.0 -host-port 7999 -container-ip 172.17.0.8 -container-port 7000
root     12789  0.0  0.0 274296  4260 ?        Sl   Aug21   0:12 docker-containerd-shim 7b4d4317d894716d833fa8a8103db33898866b6e18859d6dbe7e1d949ec1df5f /var/run/docker/libcontainerd/7b4d4317d894716d833fa8a8103db33898866b6e18859d6dbe7e1d949ec1df5f /usr/libexec/docker/docker-runc
root     20856  0.0  0.1 205064 11092 ?        Ssl  Aug21   0:00 /usr/bin/docker-latest run --rm --log-driver journald --name new-hope-search-redis redis:3
root     20947  0.0  0.0 209016  2200 ?        Sl   Aug21   0:00 docker-containerd-shim 0fd1f66e854b95168fd08e627a09cc39d092e3726f1ed7161181b8f9aeb8e031 /var/run/docker/libcontainerd/0fd1f66e854b95168fd08e627a09cc39d092e3726f1ed7161181b8f9aeb8e031 /usr/libexec/docker/docker-runc
root     21141  0.0  0.1 270600 11360 ?        Ssl  Aug21   0:00 /usr/bin/docker-latest run --rm --memory 1g --ulimit memlock=-1:-1 -e cluster.name=matrix-sentinel-cluster -e xpack.security.enabled=false -e bootstrap.memory_lock=true -e ES_JAVA_OPTS=-Xms512m -Xmx512m --log-driver journald --name new-hope-search-elasticsearch docker.elastic.co/elasticsearch/elasticsearch:5.5.0
root     21193  0.0  0.0 274296  4224 ?        Sl   Aug21   0:00 docker-containerd-shim 83a0f40af95be759eae68c260f5d9a379a0bc90cda09c111255cd7ef3d6a8e6f /var/run/docker/libcontainerd/83a0f40af95be759eae68c260f5d9a379a0bc90cda09c111255cd7ef3d6a8e6f /usr/libexec/docker/docker-runc
root     24704  0.0  0.0 216240  7984 ?        Ssl  Aug21   0:03 /usr/bin/docker-latest run --rm --link new-hope-search-redis --link new-hope-search-elasticsearch -e DB_HOST=wnu-postgres1.nzx.com -e DB_NAME=nzxcom_uat -e DB_PASS=zq12wxce34rv -e DB_PORT=5432 -e DB_USER=clear -e ELASTICSEARCH_HOST=new-hope-search-elasticsearch -e ELASTICSEARCH_PORT=9200 -e RAILS_ENV=production -e RAILS_SERVE_STATIC_FILES="true" -e REDIS_HOST=new-hope-search-redis -e SECRET_KEY_BASE=25e18548cd50d062957997fa7a4e69e52aaa3a0afa9afd19b92308c5f5fff64eb37d71be7e2934c6ee38a412ce2b6cf6fd86023080f09e23eabf94781570956e -e TZ="Pacific/Auckland" -p 6000:7000 --log-driver journald --name new-hope-search-app nzxsed/matrix-sentinel:develop ./docker_start.sh
root     24830  0.0  0.0 192088  1944 ?        Sl   Aug21   0:00 /usr/bin/docker-proxy -proto tcp -host-ip 0.0.0.0 -host-port 6000 -container-ip 172.17.0.9 -container-port 7000
root     24837  0.0  0.0 274296  4480 ?        Sl   Aug21   0:13 docker-containerd-shim a43d3896a9f5c8119644295f89332e39e780967ef6a16e579f7cd44f0edbb16e /var/run/docker/libcontainerd/a43d3896a9f5c8119644295f89332e39e780967ef6a16e579f7cd44f0edbb16e /usr/libexec/docker/docker-runc
root     24853  0.0  0.0   1508   180 ?        Ss   Aug21   0:00 /bin/sh ./docker_start.sh
root     28378  0.5  0.0 124452  9564 ?        Ssl  09:24   0:00 /usr/bin/docker-latest run --rm -p 443:8090 -e BANCS_HOST=MISSING -e BANCS_PORT=MISSING -e ENTITY_ID=MISSING -e BP_ID=MISSING -e USER_ID=MISSING -e BIC=MISSING -e KEY_PATH=MISSING -e CERT_PATH=MISSING -e TZ=Pacific/Auckland --log-driver=journald --name bancs-client docker.io/nzxsed/bancs-client:develop bundle exec /app/bin/bancs-client
root     28443  0.0  0.0 112652   960 pts/0    S+   09:24   0:00 grep --color=auto docker
root     28763  4.7  0.8 2490628 87148 ?       Ssl  Aug21  70:19 /usr/bin/dockerd-latest --add-runtime docker-runc=/usr/libexec/docker/docker-runc --default-runtime=docker-runc --authorization-plugin=rhel-push-plugin --exec-opt native.cgroupdriver=systemd -g /var/lib/docker-latest --selinux-enabled --log-driver=journald -H unix:///var/run/docker.sock -H tcp://127.0.0.1:4243 --add-registry registry.access.redhat.com
root     28772  0.7  0.1 861436 15392 ?        Ssl  Aug21  11:12 docker-containerd -l unix:///var/run/docker/libcontainerd/docker-containerd.sock --shim docker-containerd-shim --metrics-interval=0 --start-timeout 2m --state-dir /var/run/docker/libcontainerd/containerd --runtime docker-runc --runtime-args --systemd-cgroup=true
root     29218  0.0  0.1 131332 11656 ?        Ssl  Aug21   0:00 /usr/bin/docker-latest run --rm --log-driver=journald -e TCP_PORTS=11211 -p 11211:11211 --name new-hope-memcached memcached:1.4-alpine
root     29238  0.0  0.0 198532  9652 ?        Ssl  Aug21   0:00 /usr/bin/docker-latest run --rm -e TZ="Pacific/Auckland" --name redis-erdinger --log-driver=journald docker.io/redis:3.0-alpine
root     29863  0.0  0.0 205632  4328 ?        Sl   Aug21   0:26 /usr/bin/docker-proxy -proto tcp -host-ip 0.0.0.0 -host-port 11211 -container-ip 172.17.0.5 -container-port 11211
root     29886  0.0  0.0 274296  2156 ?        Sl   Aug21   0:00 docker-containerd-shim 015eb11c1e76891e990fe07e37dafe091b6b94c973aefd686fdefafb4e985f46 /var/run/docker/libcontainerd/015eb11c1e76891e990fe07e37dafe091b6b94c973aefd686fdefafb4e985f46 /usr/libexec/docker/docker-runc
root     30056  0.0  0.0 340088  2372 ?        Sl   Aug21   0:00 docker-containerd-shim 1c6b6c3957a085046da1e58003a4126571f4e11ad8d5640a866f3af754690f77 /var/run/docker/libcontainerd/1c6b6c3957a085046da1e58003a4126571f4e11ad8d5640a866f3af754690f77 /usr/libexec/docker/docker-runc
root     31469  0.0  0.1 216240 10520 ?        Ssl  Aug21   0:34 /usr/bin/docker-latest run --rm -e RAILS_ENV=production -e RAILS_SERVE_STATIC_FILES=true -e DB_HOST=wnu-postgres1.nzx.com -e DB_PORT=5432 -e DB_USER=clear -e DB_PASS=zq12wxce34rv -e DB_NAME=nzxcom_uat -e CLAG_URL=http://clag.csu-gxmarket.nzx.com -e CLAG_KEY=ingress -e ISEARCH_DOMAIN=isearch.wnc-map.nzx.com -e ISEARCH_USER=sed-services%40nzx.com -e ISEARCH_PASS=w3Y2XyqMqQdw\  -e BANCS_CLIENT=http://akp-sed-erd2.nzx.com:443 -e TZ=Pacific/Auckland -e TCP_PORTS=4000 -e DB_POOL=9 -e WORKERS=4 -p 4000:4000 --log-driver=journald --name egress-service-1 nzxsed/egress-service:develop foreman start
root     31572  0.0  0.1 354152 11440 ?        Sl   Aug21   1:00 /usr/bin/docker-proxy -proto tcp -host-ip 0.0.0.0 -host-port 4000 -container-ip 172.17.0.2 -container-port 4000
root     31581  0.0  0.0 405112  4176 ?        Sl   Aug21   1:20 docker-containerd-shim 4194cd13fc4abc489e618be0627b467561aff817961b032d8cd3dca10cec3c28 /var/run/docker/libcontainerd/4194cd13fc4abc489e618be0627b467561aff817961b032d8cd3dca10cec3c28 /usr/libexec/docker/docker-runc

# and grouping the processes by type

# general docker processes (which run even if no containers)
/usr/libexec/docker/rhel-push-plugin
/usr/bin/dockerd-latest --add-runtime docker-runc=/usr/libexec/docker/docker-runc --default-runtime=docker-runc --authorization-plugin=rhel-push-plugin --exec-opt native.cgroupdriver=systemd -g /var/lib/docker-latest --selinux-enabled --log-driver=journald -H unix:///var/run/docker.sock -H tcp://127.0.0.1:4243 --add-registry registry.access.redhat.com
docker-containerd -l unix:///var/run/docker/libcontainerd/docker-containerd.sock --shim docker-containerd-shim --metrics-interval=0 --start-timeout 2m --state-dir /var/run/docker/libcontainerd/containerd --runtime docker-runc --runtime-args --systemd-cgroup=true

# not sure what this is???
/bin/sh ./docker_start.sh

# docker run commands (one for each systemd service)
/usr/bin/docker-latest run --rm --link new-hope-search-redis --link new-hope-search-elasticsearch -e DB_HOST=wnu-postgres1.nzx.com -e DB_NAME=nzxcom_uat -e DB_PASS=zq12wxce34rv -e DB_PORT=5432 -e DB_USER=clear -e ELASTICSEARCH_HOST=new-hope-search-elasticsearch -e ELASTICSEARCH_PORT=9200 -e RAILS_ENV=production -e RAILS_SERVE_STATIC_FILES="true" -e REDIS_HOST=new-hope-search-redis -e SECRET_KEY_BASE=25e18548cd50d062957997fa7a4e69e52aaa3a0afa9afd19b92308c5f5fff64eb37d71be7e2934c6ee38a412ce2b6cf6fd86023080f09e23eabf94781570956e -e TZ="Pacific/Auckland" -p 6000:7000 --log-driver journald --name new-hope-search-app nzxsed/matrix-sentinel:develop ./docker_start.sh
/usr/bin/docker-latest run --rm -e S3_ACCESS_KEY_ID=AKIAJABUQTI7JQTRAXGA -e S3_SECRET_ACCESS_KEY=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 -e AWS_REGION=ap-southeast-2 -e CMS_BUCKET=nzx-prod-c84t3un4 -e ANN_BUCKET=nzx-prod-s7fsd7f98s -e INTERNET_PROXY=http://wnp-int-services.nzx.com:3128 -e SEARCH_URL=http://wlt-sed-nzxcom1.nzx.com:6000 -e RAILS_ENV=uat -e CLAG_URL=http://clag.csu-gxmarket.nzx.com -e CLAG_KEY=ingress -e ISEARCH_URL=isearch.wnc-map.nzx.com -e ISEARCH_USER=sed-services%40nzx.com -e ISEARCH_PASS=w3Y2XyqMqQdw -e SECRET_KEY_BASE=05256d0bfb8bbead806f742b792da8533818524bcfd2147e963d37cddab977042d2441e133fa0ba90c1aaa2b90eeb18a0419482374bc395e099b0f4549faa254 -e RAILS_SERVE_STATIC_FILES=false -e TZ=Pacific/Auckland -e DB_HOST=wnu-postgres1.nzx.com -e DB_PORT=5432 -e DB_NAME=new_hope_uat -e DB_USER=deploy -e DB_PASS=deploy -e CACHE_HOST_01=new-hope-memcached:11211 -e INGRESS_URL=http://wlt-sed-nzxcom1.nzx.com:4000 -e DB_POOL=24 -e WORKERS=2 -e RAM=1024 -p 7000:7000 --link new-hope-memcached --log-driver=journald --name new-hope-1 nzxsed/a-new-hope:develop foreman start
/usr/bin/docker-latest run --rm -e S3_ACCESS_KEY_ID=AKIAJABUQTI7JQTRAXGA -e S3_SECRET_ACCESS_KEY=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 -e AWS_REGION=ap-southeast-2 -e CMS_BUCKET=nzx-prod-c84t3un4 -e ANN_BUCKET=nzx-prod-s7fsd7f98s -e INTERNET_PROXY=http://wnp-int-services.nzx.com:3128 -e SEARCH_URL=http://wlt-sed-nzxcom1.nzx.com:6000 -e RAILS_ENV=uat -e CLAG_URL=https://auth.nzx.com -e CLAG_KEY=44e3901fc4c09d60508f18ee639a5800486b12b1 -e ISEARCH_URL=isearch.wnc-map.nzx.com -e ISEARCH_USER=sed-services%40nzx.com -e ISEARCH_PASS=w3Y2XyqMqQdw -e SECRET_KEY_BASE=05256d0bfb8bbead806f742b792da8533818524bcfd2147e963d37cddab977042d2441e133fa0ba90c1aaa2b90eeb18a0419482374bc395e099b0f4549faa254 -e RAILS_SERVE_STATIC_FILES=true -e TZ=Pacific/Auckland -e DB_HOST=wnu-postgres1.nzx.com -e DB_PORT=5432 -e DB_NAME=new_hope_uat -e DB_USER=deploy -e DB_PASS=deploy -e CACHE_HOST_01=new-hope-memcached:11211 -e INGRESS_URL=http://wlt-sed-nzxcom2.nzx.com:4000 -e DB_POOL=24 -e WORKERS=2 -e RAM=1024 -p 7999:7000 --link new-hope-memcached --log-driver=journald --name new-hope-2 nzxsed/a-new-hope:develop foreman start
/usr/bin/docker-latest run --rm --log-driver journald --name new-hope-search-redis redis:3
/usr/bin/docker-latest run --rm --memory 1g --ulimit memlock=-1:-1 -e cluster.name=matrix-sentinel-cluster -e xpack.security.enabled=false -e bootstrap.memory_lock=true -e ES_JAVA_OPTS=-Xms512m -Xmx512m --log-driver journald --name new-hope-search-elasticsearch docker.elastic.co/elasticsearch/elasticsearch:5.5.0
/usr/bin/docker-latest run --rm -p 443:8090 -e BANCS_HOST=MISSING -e BANCS_PORT=MISSING -e ENTITY_ID=MISSING -e BP_ID=MISSING -e USER_ID=MISSING -e BIC=MISSING -e KEY_PATH=MISSING -e CERT_PATH=MISSING -e TZ=Pacific/Auckland --log-driver=journald --name bancs-client docker.io/nzxsed/bancs-client:develop bundle exec /app/bin/bancs-client
/usr/bin/docker-latest run --rm --log-driver=journald -e TCP_PORTS=11211 -p 11211:11211 --name new-hope-memcached memcached:1.4-alpine
/usr/bin/docker-latest run --rm -e TZ="Pacific/Auckland" --name redis-erdinger --log-driver=journald docker.io/redis:3.0-alpine
/usr/bin/docker-latest run --rm -e RAILS_ENV=production -e RAILS_SERVE_STATIC_FILES=true -e DB_HOST=wnu-postgres1.nzx.com -e DB_PORT=5432 -e DB_USER=clear -e DB_PASS=zq12wxce34rv -e DB_NAME=nzxcom_uat -e CLAG_URL=http://clag.csu-gxmarket.nzx.com -e CLAG_KEY=ingress -e ISEARCH_DOMAIN=isearch.wnc-map.nzx.com -e ISEARCH_USER=sed-services%40nzx.com -e ISEARCH_PASS=w3Y2XyqMqQdw\  -e BANCS_CLIENT=http://akp-sed-erd2.nzx.com:443 -e TZ=Pacific/Auckland -e TCP_PORTS=4000 -e DB_POOL=9 -e WORKERS=4 -p 4000:4000 --log-driver=journald --name egress-service-1 nzxsed/egress-service:develop foreman start

# proxy commands - one for each port mapping between host and container
/usr/bin/docker-proxy -proto tcp -host-ip 0.0.0.0 -host-port 11211 -container-ip 172.17.0.5 -container-port 11211
/usr/bin/docker-proxy -proto tcp -host-ip 0.0.0.0 -host-port 4000 -container-ip 172.17.0.2 -container-port 4000
/usr/bin/docker-proxy -proto tcp -host-ip 0.0.0.0 -host-port 6000 -container-ip 172.17.0.9 -container-port 7000
/usr/bin/docker-proxy -proto tcp -host-ip 0.0.0.0 -host-port 7000 -container-ip 172.17.0.3 -container-port 7000
/usr/bin/docker-proxy -proto tcp -host-ip 0.0.0.0 -host-port 7999 -container-ip 172.17.0.8 -container-port 7000

# one containerd-shim process for each container
docker-containerd-shim a43d3896a9f5c8119644295f89332e39e780967ef6a16e579f7cd44f0edbb16e /var/run/docker/libcontainerd/a43d3896a9f5c8119644295f89332e39e780967ef6a16e579f7cd44f0edbb16e /usr/libexec/docker/docker-runc
docker-containerd-shim 015eb11c1e76891e990fe07e37dafe091b6b94c973aefd686fdefafb4e985f46 /var/run/docker/libcontainerd/015eb11c1e76891e990fe07e37dafe091b6b94c973aefd686fdefafb4e985f46 /usr/libexec/docker/docker-runc
docker-containerd-shim 1c6b6c3957a085046da1e58003a4126571f4e11ad8d5640a866f3af754690f77 /var/run/docker/libcontainerd/1c6b6c3957a085046da1e58003a4126571f4e11ad8d5640a866f3af754690f77 /usr/libexec/docker/docker-runc
docker-containerd-shim 7b4d4317d894716d833fa8a8103db33898866b6e18859d6dbe7e1d949ec1df5f /var/run/docker/libcontainerd/7b4d4317d894716d833fa8a8103db33898866b6e18859d6dbe7e1d949ec1df5f /usr/libexec/docker/docker-runc
docker-containerd-shim 0fd1f66e854b95168fd08e627a09cc39d092e3726f1ed7161181b8f9aeb8e031 /var/run/docker/libcontainerd/0fd1f66e854b95168fd08e627a09cc39d092e3726f1ed7161181b8f9aeb8e031 /usr/libexec/docker/docker-runc
docker-containerd-shim 83a0f40af95be759eae68c260f5d9a379a0bc90cda09c111255cd7ef3d6a8e6f /var/run/docker/libcontainerd/83a0f40af95be759eae68c260f5d9a379a0bc90cda09c111255cd7ef3d6a8e6f /usr/libexec/docker/docker-runc
docker-containerd-shim cbd2afc36c99f475ceb7cd304c5c680be28ba380bc2bde21ed182ebf5a162b22 /var/run/docker/libcontainerd/cbd2afc36c99f475ceb7cd304c5c680be28ba380bc2bde21ed182ebf5a162b22 /usr/libexec/docker/docker-runc
docker-containerd-shim 4194cd13fc4abc489e618be0627b467561aff817961b032d8cd3dca10cec3c28 /var/run/docker/libcontainerd/4194cd13fc4abc489e618be0627b467561aff817961b032d8cd3dca10cec3c28 /usr/libexec/docker/docker-runc
```
