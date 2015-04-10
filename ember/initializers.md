
# The diff between initializers and instanceInitializers

* https://github.com/emberjs/website/pull/1951

> To be concrete: the problem is that it is important for all app-wide
> injection rules to be set up before any instances are created through the
> container

> In the new design, which this deprecation guide is helping people transition
> to, App.initializers ("boot initializers") only have access to a registry,
> which they can use to set up injections. App.instanceInitializers have access
> to the container, which they can use to instantiate objects. All boot
> initializers are guaranteed to run before the instance initializers.

there is an ordering issue that they want to solve

initializers
    * have access to only a registry
    * can only setup injections
instance initializers
    * have access to a registry _and_ a container
    * can create instances of objects in the container

Ember will run all initializers _before_ any instance initializers to garuantee

You want to do all your "registering" of things before you do any "lookup" of things

## from the feature flag doc:

Splits apart initializers into two phases:

Boot-time Initializers that receive a registry, and use it to set up code structures
Instance Initializers that receive an application instance, and use it to set up application state per run of the application.
In FastBoot, each request will have its own run of the application, and will only need to run the instance initializers.

In the future, tests will also be able to use this differentiation to run just the instance initializers per-test.

With this change, App.initializer becomes a "boot-time" initializer, and issues a deprecation warning if instances are accessed.

Apps should migrate any initializers that require instances to the new App.instanceInitializer API.
QUESTION: what is the diff between container and registry? (Sat  4 Apr 10:09:20 2015)

