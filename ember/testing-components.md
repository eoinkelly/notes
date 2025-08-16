# creating objects for test

```js
var comp = App.MyFooComponent.create().append();
comp.container = App.__container__; // allow lookups from the component back to the container to work
```

# Consequences of this working

- ember doesn't need to find it in the container if I manually append it to the
  DOM
- the component uses its `container` attribute to find the app container
