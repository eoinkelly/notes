
https://developer.mozilla.org/en/docs/Web/CSS/Attribute_selectors

save your overrides in the app/overrides, 
normally one override per file 
using the same file name as specified in the :name argument.

the required args have three sections

* target
    * `:virtual_path`
        * the template that the override should take effect
        * is the segement of the view path: `app/views/{virtual path}.html.erb`
* action
    * the action to take on the targetted node
    * `replace: "selector"
        * selector can match multiple times in the targetted template
* source
    * where to find the new nodes
    * can be inline string, another template or a partial
    * can be a cut/copy/paste from some other targetting!

optional args

* name
    * tag the override with a name so it can be referred to later
    * needs to be unique within the same virtual path
* attributes
    * XML attributes to be set on the replacement node
    
In the DSL the filename provides the `virtual_path` and `name` attribute

```
app/overrides/{virtual path}/{name}.deface
```

```ruby
Deface::Override.new(:virtual_path => "posts/_form", 
                     :name => "example-1", 
                     :replace => "h1", 
                     :text => "<h1>New Post</h1>")
```


