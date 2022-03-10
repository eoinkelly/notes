# QEMU

* generic machine emulator and virtualizer
* 2 Modes
    1. System emulation
        * qemu provides a virtual model of an entire machine (CPU + memory + devices) to run a guest OS
        * CPU may be fully emulated or may work with a hypervisor
    2. User mode emulation
        * can launch a process compiled for one CPU on another CPU
        * CPU is always emulated
* Used by docker desktop to run x86 images on M1 ARM
* Docker have forked QEMU
  * it runs within their linux VM to run cross-platform binaries
* Docker Desktop has `buildx` which lets you build cross-platform docker images and push them to registry


## Hypervisor

* also called VMM Virtual Machine Monitor
* software that creates and runs guest virtual machines by sharing it's resources

### Type 1 (Bare metal)

* acts as a lightweight operating system
    * smaller attack surface
    * don't waste resources by giving them to the host OS
    * much more common than Type 2
* runs directly on the host hardware
* used in data centers
* sometimes embedded into the firmware of the motherboard like the BIOS
* e.g. Xen

### Type 2 (Hosted hypervisor or Client hypervisor)

* runs as a software layer on a host OS like any other process
* used for local testing on individual desktop machines
* e.g. Virtualbox

### Hypervisor examples

* KVM (type 1)
    * https://www.linux-kvm.org/page/Main_Page
    * kernel based virtual machine
    * is a kernel module
    * the kernel it is installed in becomes the hypervisor
    * is a type-1 hypervisor
    * every guest runs as a seprate systemd process
    * depends on processor having hardware virtualisation extensions
* Xen (type 1)
    * Type 1 (bare metal) hypervisor
    * guest OSes are called "domains"
    * it runs one guest as a controller for itself - called the "Control domain"a or `dom0`
        * the control domain can talk to the hypervisor
        * the control domain provides the device drivers needed to access the hardware
        * the hypervisor has no drivers!
        * the hardware is actually attached to dom0 and then the hypervisor accesses it through dom0
        * dom0 shares these resources with other guests
    * installation (roughly speaking)
        1. install debian (this becomes dom0)
        2. install xen package in debian
        3. reboot and choose xen from the grub menu
* Hax (type ???)
    * https://github.com/intel/haxm
    * used by Android Emulator and QEMU
    * runs as a kernel-mode driver on the host
    * provides a KVM-like interface to user space (QEMU uses this)
    * runs on Windows, macOS, others
* Hypervisor.Framework (type ???)
    * macOS only
    * https://developer.apple.com/documentation/hypervisor
    * lets you create and manage a guest without needing macOS kernel extensions
    * tools which run on it:
        * https://github.com/machyve/xhyve
        * https://mac.getutm.app/
            * runs QEMU under the hood with a layer of macOS GUI on top
* Virtualisation.Framework (type ???)
    * alternative to Hypervisor.Framework
        * higher level - has Swift APIs where as Hypervisor is C APIs only
    * arrived in BigSur
    * currently an experimental option in Docker Desktop (reputedly buggy and slower but maybe uses less RAM?)
