
# Kawaiicon 2022 Peter Gutman talk


Fault-resistant systems are described as _X out of Y_ (abbreviated _XooY_) - X of the Y units must fail for a system failure

* 1oo1
	* standard systems are 1oo1
	* failure in any part causes system failure
* 1oo1D
	* 1oo1 with a diagnostic channel
	* fail-fast system
	* if failure detected by the monitoring system, halt or restart the main system
	* common in SCADA systems
* 1oo2 system
	* fail safe system
	* both systems must fail for an inadvertent activation to occur
	* requires custom hardware
* 2oo2 system
  * high availability
  * both systems must fail for overall failure
	* requires custom hardware
* 2oo3 with voting circuits
	* requires custom hardware

## Can we make general purpose software 1oo1D?

Do this with the _Swiss cheese model_ of fault prevention

* defenses are imperfect
* with enough layers, a fault has to go all the way through
* need to constrain the control and data flow in such a manner that error propagation through the entire system is highly unlikely

How do we make this part of programming practice?

> Add enough layers of constraints to ensure that faults moving processing outside the permitted envelop is unlikely

i.e. Design by Contract

* blocks of codes have pre-conditions and post-conditions which are checked at runtime
* This is well supported by languages like Eiffel
* Can be done in C using macros
* You can implement checks on values at **runtime** to ensure they are valid - these checks must happen at runtime because the are guarding against runtime issues.
* Most C compilers will optimize away any runtime checks you put on the bounds of loops
* You can pass flags to gcc to not do this but it has bad code generation
* Standards like https://www.iso.org/standard/68390.html warn against using optimizing compilers because optimization breaks the mapping between source code and compiled output