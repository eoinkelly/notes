CPU allocation is proportional to your RAM allocation rumour is that you get
multiple cores around 1.5 GB

https://www.youtube.com/watch?v=QdzV04T_kec

- AWS uses Amazon Linux as the "guest OS" i.e. the base OS for running sandboxes
    - multiple functions in one account will share a guest OS (the guest OS runs
      a sandbox per function)
    - Guest OS is **never** shared across multiple AWS accounts
- Lambda "sandbox" is a fairly full featured copy of linux
    - each function gets its own sandbox
    - a sandbox is never re-used across functions
    - sandbox uses the same tech that containers use to achieve isolation
        - cgroups
        - namespaces
        - iptables
        - chroot
        - seccomp bpf
            - allows you to lock down which syscalls the sandbox can use (or
              which args can be passed to certian syscalls)
    - Your lambda function always runs as PID 1 within the sandbox

Two kinds of Lambda workers

1. Nitro
    - the workers are just EC2 instances
1. Firecracker
    - Bare metal EC2 instance running a Host OS and hypervisor. The hypervisor
      runs many micro VMs, each microvm runs the Guest OS which in turn runs the
      sandboxes
