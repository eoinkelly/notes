# @jimweirich play by play

## DelegateClass

```
class Foo < DelegateClass(User)
end
```

- `DelegateClass` does not seem to exist as a global method. I think it is
  probably some trickery on the `<` keyword that makes it work

## Given library

## Misc tips

- jim likes to have short branches first in an if-else as it makes things more
  readable
- `.git/info/exclude` is good for stuff you want to exclude but don’t want to
  check the exclusion into the project

## `Object#methods` Vs `Module#instance_methods`

### Module.instance_methods

- defined on Module - so it is available to all objects of type Module, Class
- gets public and protected instance methods in the receiver
- for a module, these are the public and protected methods
- for a class, they are the the instance (not singleton) methods
- Thing.instance_methods(false) # pass false to not get methods from the
  ancestors array

### How do I see what methods are defined by a class/module (ignoring anything in it’s ancestors)?

```
Thing.instance_methods(false)
```

### Object#methods

- returns a list of names of public and protected methods of the receiver. This
  includes \* all methods defined in all all ancestors - this will include
  methods from singleton classes
- pass it false as param to get methods defined in the singleton classes
- ? does it just do the immediate singleton classes or could there be multiple
  layers of singleton classes?

### How do I see what methods are defined in the singleton class of a given object?

```
thing.methods(false)
```

How do I see all the public and protected methods are available to a given
object including those in the singleton class?

```
thing.methods
```
