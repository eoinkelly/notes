
## dpkg -l <pkg-name> output

The first column corresponds to the status of a package.

Status of every package is represented by three characters xxx

First character: Desired state

* u: Unknown (an unknown state)
* i: Install (marked for installation)
* r: Remove (marked for removal)
* p: Purge (marked for purging)
* h: Hold

Second Character: Current state

* n: Not- The package is not installed
* i: Inst – The package is successfully installed
* c: Cfg-files – Configuration files are present
* u: Unpacked- The package is stilled unpacked
* f: Failed-cfg- Failed to remove configuration files
* h: Half-inst- The package is only partially installed
* W: trig-aWait
* t: Trig-pend

Third Character: Error state

* R: Reinst-required The package must be installed.
