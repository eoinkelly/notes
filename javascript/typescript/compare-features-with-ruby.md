
Map Ruby features to Typescript


## Class

```ruby
class Thing
end
```

```ts
class Thing {
    // private static method
    static example() { }

    // public sttic method
    public static example() { }
    static example() { }
}
```

* Ruby
    * A constant
    * In a global namespace
    * created when file is required
    *
    * Available when:
        * you are in same file
        * you `require` it from its file
* Typescript
    * Syntax sugar for a function (I think)
    * prefix member (funciton or variable) with `static`
    * Available when:
        * you are in same file
        * you `import` it from its module file

### public class method (static method)

### private class method (static method)
* ruby: requires a somewhat hacky syntax
* ts:

Class : Class
static method on class: static method on class
private static method (hacky syntax): MISSING
public class variable: ???
private class variable: ???
public instance method : public instance method
private instance method : naming convention
instance variable through public getter/setter: instance variable through public getter/setter:
private instance variable: naming convention