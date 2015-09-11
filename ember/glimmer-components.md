
* aka angle bracket components
* enabled 1-way binding by default

```hbs
<my-awesome-thing src={{model.foo}} />
```


component.rerender()
component.set()

new component hooks

didInsertElement
    * old hook
    * fired in response to an event from outside the component (parent
      inserting it into DOM)
didUpdateElement
    * new hook
    * fired when changes come from inside the component


attrs
<my-comp src={{model.src}} />

within the component you access `attrs.src`


# mut helper

Since bindings passed into templates are read-only by default we need a way of
updating them directly - the mut helper does this by wrapping an attr you pass
in in another object.

Consider:

    <my-comp src={{model.src}} />

which puts a read-only reference to `model.src` in `attrs.src` within the
component. Now consider

    <my-comp src={{mut model.src}} />

`mut` is a _helper_ that produces an object that like

```js
{
    value: model.src
    update: function(new) {
        // pseudocode:
        parent.get('model.').set('src', new)
    }
}
```

within the componetn you read the mutable thing as

    this.attrs.src.value

and write it as

    this.attrs.src.update(newVal)

This has the side-effect of making it obvious when you are changing attrs you
were passed in
