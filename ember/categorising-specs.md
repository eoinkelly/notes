Q: What makes a spec be "integration" vs "unit"?

From ember docs:
> unit tests do not require the application to be running

factors

* depends on the whole, working App being available
* depends on getting stuff out of the Ember container
* depends on ember test helpers (which are injected into the global scope)
* depends on a DOM being available

access patterns
access the test subject via it's JS interface
access the test subject via the real, whole, application DOM
access it via a fixture DOM
