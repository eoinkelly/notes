# toString()

Ember provides toString() on all its objects

https://github.com/emberjs/ember.js/blob/v1.7.0/packages/ember-runtime/lib/system/core_object.js#L389

Format is

```
<{constructor}:{guid}{:extension}>
```

* `{constructor}`
    * `this.constructor.toString()`
    * value is the toString of the factory object that created `this`
* `{guid}`
    * `guidFor(this)`
* `{extension}`
    * the return value of this.toStringExtension() if you have defined
      that function for the current object
    * extension for Ember-data object seems to be the object ID

Ember.guidFor()
    * returns a guid for the object instance
    * creates one if it doesn't already have one
    * works on normal JS things and on ember things

    (true)
    (false)

    prefixes
        st = string
        nu = number
        ember = ember things

    * for ember objects returns 'ember' + uuid()

```js
"eoin" + (+ new Date())
```
