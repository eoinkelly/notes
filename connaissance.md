# Connascence

* a particular kind of coupling


Defn:

* Things which are born together and grow together
* the common birth of two or more at the same time
* the production of two or more together
* the act of growing together
    * => so it refers more to the fact that twins grow up together rather than that they were born at the same time

Two things are connascence if a change in one would require a change in the other to maintain system correctness

### Types of connascence (from weakest to strongest)

@jweirich feels the connascences are in a heirarchy of some kind but he doesn't
feel comfortable assigning a precise ordering. He seems to believe connascence
of name is the "least worst" kind.

* Static:
    1. name (weakest)
        * a connection between the definition of a symbol and *all* of the call sites that use it
        * occurs whenever two components must agree on the same name
    2. type
        * occurs when two components must agree on the same type
        * defn: A type is 2 related sets:
            1. a set of data values
            2. a set of operations on those values
        * examples
            * a stack:
                1. a set of values where each value knows what value comes next
                2. a set of operations: `push()` `pop()`, `empty()`
        * connascence of type includes other connascences:
            1. connascence of name in "manifestly typed" languages like Java but not langs like Ruby
                * In Java you have to say the name of the type you are using in all scenarios
            2. connasenc of name and/or position of parameters passed to the methods in the type
            3. connascence of "semantics"/"algorithm"/"interface"
                * e.g. if I `push('xx')` and `pop()` does not return `xx` the the stack is "broken" for me.
                * Explicit interfaces state the connascences of name of the
                  type, name & position of parameters but they don't specify
                  this connascence of "expectation" at all.
            * connascence is a very complex summary of the others
                ```
                # "pseudo equations"
                conn_of_params = num_params * conn_name * conn_position
                conn_of_type = num_methods * (conn_name_of_type + conn_of_params + conn_of_expectation)
                ```
        * @jimweirich things that connascence of type is probably weaker in languages which don't have manifest types
    3. meaning
        * when two modules have to agree on the interpretation of a particular value
        * when a particular data value has to be _interpreted_ consistently
        * examples:
            * checking for "4111111111111111" in your code to identify
            * `Fixnum.instance_methods(false)`
                * we have assigned particular meaning to `true` and `false` but
                  in order for the system to work we have to share that
                  interpretion
            * returning nil as failure value - we have to agree on what nil means here e.g. `User.find(3)`
                * found no record?
                * encountered an error?
        * control coupling is a form of connascence of meaning
            * can happen in 2 directions
            1. into method
                * passing some data into a method that controls *how* it does its job very explicitly
                * occurs when one component passes in a piece of info that is
                intended to control the internal *logic* of the other.
                * jimweirich thinks it is not totally good/not totally evil
                * smell:
                    * if your method name includes `or`
                    * passing in data _with no intrinsic meaing_ that is not intrinsic to the project e.g.
                        * true|false
                        * symbols that tweak the algorigthm e.g. :first, :all
                        * nil
            2. out of method
                * returning some value(s) that control the internal logic of the component that calle dyou
        * remedies
            * you can replace the value with a shared constant
                * => you go from meaning to name which is better (not perfect)
    4. position
        * examples:
            * positional method args
            * SQL select statements give you back the columns in the order you
              specified e.g. `SELECT a, b, c FROM foo;`
    5. algorithm
        * when two components must agree on a particluar algorithm
            * example
                * compression scheme: if I compress with gzip then some other part needs to know that
                * checksums
                * crypto
                * shared regular expressions for parsing
                * rails mapping incoming URLs to Controller#action and rails link_to helpers are linked by the "URL algorithm"
                    * we reduce this to connascence of name with a helper method
* Dynamic
    6. execution
        * when the order of execution of two components is important
        * the ordering of execution changes the outcome
        * when excutions must happen in a particular order
        * pure functions don't have this problem - is this totally true?
    7. timing
        * connascence of timing where the timing of execution affects the outcome of the program
        * examples
            * timeouts
            * race conditions
    8. value
        * when the values of two components are related
        * if you have a program that builds a triangle from 3 ints, there are
          possible values for the ints that will make the value of the triangle
          invalid
            * you cannot see that in the code!
    9. identity (strongest)
        * when two components must refer to the same object
        * @jimweirich gives example of ActiveRecord loading multiple copies of same model into memory

### Aside: data with no intrinsic meaning

* symbol
* true|fale
* nil

have no intrinsic meaning.

* They only mean whatever our program has decided they should mean
* It is impossible to use one of these and not introduce a connascence of meaning
* you can return true|false from a `foo?` method and this is _data_ but if the true|false controls some internal logic of the caller then it is connascence of meaning.
* Because these have no intrinsic meaning themselves we give them meaning in our app
    * that meaning can introduce a connascence of meaning if two separate components have to agree on it

## Static vs Dynamic

* Static => you can see the conascence from inspecting the code
* Dynamic => you can only see the conascence at runtime

### Principles

* Degree
    * Each type will also have a "degree" which indicates how often it appears in your system
    * The goal is to move stronger ones to weaker ones as much as possible
* Locality
    * closer conascences are easier to deal with so try to minimize conascenc at big distances
* Stability


DRY and SRP try to keep locality manageagel
depend on things which are stable - Connascence is more acceptable on things that are very stable

# Contranascences

* when two components must agree on different names
* examples
    * inheritance: not accidently overwriting parent stuff in child


