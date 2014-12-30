# Blocks

Blocks are designed to work with C, C++ and ObjectiveC (this may the source of the strange syntax)


Block inputs:

* The arguments you provide
* Any variables in the scope surrounding the block

A block is a function and an object - it is a chunk of executable code and some state. A block is a function and an object - it is a chunk of executable code and some state.

What are the scopes in ObjC?

* Global
    * any variables declared outside function
* Current file
    * variables declared outside function and limited with `static`
* Class
    * instance variables are visible to all methods in the class
* Function
    * variables declared within or parameters passed to a function


## Variable capturing

A block is said to "capture":

1. local variables of the mehtod it was defined in
2. arguments passed to the method it was deined in
3. instance variables belonging to the object owning the method

"capture" means that the variable is _copied_ into the memory of the block.
This means the block gets its own copy so the original is not mutated (this is
a key diff to how functions in JS work).

Captured variables remain the same _every time the block is called_! The
arguments to a block can change between calls but the captured variables are
frozen when the block is created.

Captured variables are "frozen" by default i.e. you will get an error if you
try to modify them in the block

* You can "unfreeze" them with by adding `__block` to the declaration of
    the variable `__block {type} {varname};`
* `__block` reads as "make this variable mutable within any block that has
    access to it"
* It causes the variable to be moved to a separate place in memory (it is
    not on stack or stored insidethe block itself)
* NB: marking a variable iwth `__block` means it is shared between the
    function context *and* every block within the function
    * changes outside a block will be reflected in it too

A block keeps a strong reference to any object it references from its outer
scope. This means that object is not released until the block itself goes out
of scope

You have to explicitly mark outer variables with `__block` if you want the
block to be able to mutate them (this prevents the copying)

# Block declarations

An ordinary object has a single type e.g. `NSObject`, `NSArray`. The
declaration has 2 bits of info:

1. name of variable being declared
2. type of variable being declared

and is of the form:

```objc
// {type} {name};
NSArray *as; // type = NSArray *, name=as
NSString *str; // type = NSString *, name=str
```

A block variable declaration needs to identify

1. The return type of the block
2. The number, order and type of each argument to the block
3. The name of the variable being declared

```objc
// {return type} (^{name})({arg1 type}, {arg2 type} ...);
// note the ^ is inside the parens (unlike in the block literal syntax)
void (^doStuff)(void);
int (^doubler)(int);
int (^adder)(int, int);
void (^doThings)(NSArray *);
```

# Block literals

These are block _values_.

```
^int(int x, int y) {
    // ...
    return x;
}

^{return type}({arg1 type} {arg1 name}, {arg2 type} {arg2 name} ...) {
    // contents of function
}

// compiler can figure out return type by inspecting the return statement in the block
^(int x, int y) {
    // ...
    return x;
}
```

The compiler can figure some stuff out for block literals:

* compiler can figure out return type by inspecting the return statement in the block
* you don't have to specify block args if there aren't any so this:

```
// block literal syntax for block that takes no args (return type inferred by inspection)
^{
    // do stuff
    return x;
}
```

Note that this is not true for block type declarations - they must be complete.


# Blocks as method args

```objc
- (void)doStuff: (int (^)(int, int))theBlock;
```

Notice that this syntax is subtly different to the block as a type

# Running a block

Blocks are run just like a vanilla C function e.g.

```objc
int retval = doThing(2, 4);
```


# Uses

Often used as _Completion block_ (a one-show callback to run when an operation is complete). This is a more lightweight alternative to setting up a target/action pair or a delegate

Can be used to

* queue up tasks
* spread tasks across CPU cores

# Callback patterns

1. Delegate
    * The "target" gives the "sender" a reference to itself
    * The sender calls methods on the target that are within the protocol
1. Target/action
    * The "sender" has a reference to the "target"
    * The "sender" has a SEL that it can invoke on the target
1. Notifications
    * Good when ???
1. Subclassing
    * Used in langs other than ObjC
1. Completion block
    * pro: blocks are good for one-shot stuff because they keep a strong ref to the object that created them so it will be available for the block to use and then released when the block is done.
    * pro: blocks can be defined inline in the code where they are used

In the first 3 patterns above, the target is a real method on a particular
object
