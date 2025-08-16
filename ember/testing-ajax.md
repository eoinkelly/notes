# Testing AJAX with Ember

?

https://www.npmjs.org/package/ic-ajax

- seems good

```js
// overwrite ajax to use ic.ajax
App.ApplicationAdapter = Ember.ActiveModelAdapter.extend({
  ajax: function (url type, options) {
    options = this.ajaxOptions(url, type, options);
    return ic.ajax(options);
  }
})
```
