
## How to enable a new feature:

```javascript
// this should be loaded before you load Ember
EmberEnv = {
  FEATURES = {
    'query-params-new': true
  }
}
```


* `EmberENV` is set first


* Features are available in canary builds
* Features are also in beta and release ??? but don't do this because none of the bugfixes for them will go into beta

* feature flag is an if block used by the Ember build process
* they use feature flags to try new stuff without having semver go nuts

### features.json

* You can tell what features are enabled look at `features.json` in the ember repo.
* Within `features.json`:
    * true = feature is enabled and the surrounding if statemetns have been removed
    * false = feature is disabled and the if statements + their contents are removed (feature totally gone)
        * you cannot enable this at runtime
    * null = feature is disabled but the code for it is still there
        * you can enable it at runtime

Canary: all features have 'null' value so can be enabled
Beta & Release: all features ???

Questions:

can i turn on features in release ???

