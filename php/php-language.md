# PHP language cheatsheet

Tip: Install `psysh` because the built-in PHP interactive shell is annoying e.g. it doesn't echo return values

## General

* All PHP files must have opening tag `<%php`
    * if file is all PHP content then omit closing tag
* all statements (except last one in block) require `;` termination
* comments
    * examples
        ```php
        <?php
        // single line
        /* block style */

        # also single line but these seem much less common in the PHP code I have seen
        ```
* supports anonymous functions (via the Closure class) which are most useful as callback function
    ```php
    <?php
    // creates an instance of Closure class
    $greet = function($name) {
    };

    $greet("hi");

    $outside = 12;

    $thing = function($param) use ($outside) {
        // $outside is available in here now
    }
    ```
* PHP is dynamically typed not statically typed
* It supports explicit type casting e.g. `(string)`, `(float)` etc.
* It will do automatic type coercion if necessary
    * These rules are not always obvious ...


## PHP has 10 primitive types

All type names are lowercase

1. boolean
    * use the (case insenstive) constants `TRUE` and `FALSE`
    * cast to boolean with `(bool)` or `(boolean)`
    * expressions are automatically cast to boolean if passed to a control structure
    * falsy values
        1. 0
        2. -0
        3. FALSE itself
        4. ""
        5. "0"
        6. 0.0
        7. -0.0
        8. []
        9. NULL (including unset variables)
       10. SimpleXML objects created from empty tags
2. integer
    * examples
        ```php
        <?php
        $a = 123; // decimal
        $a = 0123; // octal (GOTCHA!)
        $a = 0x123 // hex
        $a = 0b001111 // binary
        ```
    * convert to integer
        ```php
        <?php
        $a = (int) some_value;
        $a = (integer) some_value;
        $a = intval("0123") // 123 (uses base 10 by default)
        $a = intval("0123", 8) // 0o83

        (integer) FALSE // => 0
        (integer) TRUE // => 1

        ```
3. float (aliased as "double" for historical reasons)
    * converting other types to float happens by first converting them to an `integer` and then to `float`
4. string
    * literal syntaxes
        1. single quotes does not interpret escape sequences like `\n` or expand variables
        2. double quotes interprets escape sequences and expands variables
        3. heredoc
            * uses double quote expansion
        4. nowdoc
            * similar syntax to heredoc but **no parsing is done inside the nowdoc**
            * useful for embedding chunks of code or other text that you don't wnat to parse with PHP
    * can be treated like an array of bytes
    * concatenate with the `.` operator
    * variable expansion
        ```
        <?php
        $a = 12;
        echo "hello $a"; // simple syntax
        echo "hello ${a}bc"; // complex syntax
        echo "hello {$a}bc"; // complex syntax, the '{' must be followed by '$' to be recognised

        # heredoc syntax
        echo <<<'EOM'
        stuff $a
        'EOM';

        // note the semicolon after EOM

        # nowdoc
        echo <<<'EOM'
        no parsing in here
        EOM;
        // note the semicolon after EOM
        ```
    * PHP strings are implemented as `(length, array of bytes)`
        * there is no `byte` type in PHP - strings play this role
    * Strings take their encoding from the encoding of the source PHP file
    * PHP strings are not unicode safe by default!
    * The `intl` and `mbstring` extensions contain functions which work correctly with multi-byte strings
    * There doesn't seem to be a clean way to iterate over a multibyte string. TODO
5. array
    * is an **ordered** **map**
    * more like `Hash` in Ruby where if you omit the keys it will fill in integers
    * keys can be of mixed types as can values
    * built with the `array()` language construct (note: not a function) or `[]` (since PHP 5.4)
    * can have trailing commas
    * both `[]` and `{}` can be used to access array values
    * use `unset()` to remove a key/value pair
    * when an array is copied the reference status of the members is preserved
        * TODO: clarify
6. objects
    * CAREFUL NOW: Unlike Ruby, Objects are a completely different kind of thing to other primitive types
    * The PHP equivalent of Ruby's `Object.new` is `$genericObject = new stdClass();`
    * Uses `->` to access functions and attributes within the object
    * No naming rules about what can be an object class name but an intial capital seems common
        ```php
        <?php
        class Blah extends Other implements SomeInterface {
            public function foo() {
            }
            public static function foo() {
            }
            protected function foo() {
            }
        }
        ```
    * other primitive types can be converted to objects by casting via `(object)`
        * a new instance of `stdClass` is created for them when you do this
        * an array gets converted to an object where the object property names and values are taken from the array keys and values
        * examples
            ```php
            <?php
            $ob = (object) ["name" => "Eoin", "age" => 40];
            ```
7. callable
    * is strictly speaking a _type hint_ - it covers multiple kinds of function
    * can be used in function sigatures which accept a callback e.g.
        ```php
        function call_stuff(callable $thing_to_call) {
            call_user_func($thing_to_call);
        }
        ```
    * PHP supports callback functions e.g.
        ```php
        call_user_func()
        usort()
        ```
    * Callback functions can be many types
        * A plain function is passed by passing its name as a string
            * `"function_name"`
        * An object method is passed as an array containing the object at 0 and a string of the method name at 1
            * `array($ob, "method_name")`
        * A static method can be passed as
            * `array("Classname", "method_name")`
            * `array("Classname", "method_name")`
            * `"Classname::method_name"`
        * you can pass just an object if it implements the `__invoke` magic method
    * Anonymous functions can also be passed
8. iterable
    * = array|object which implements Traversable interface
    * a pseudo type (but it can be used in code)
    * introduced in 7.1
    * `iterable` objects can be used with `foreach` language construct
9. resource
    * a special variable type to hold a reference to an external resource
    * examples
        * handles to open files
        * DB connections
        * image canvas areas
        * all: https://www.php.net/manual/en/resource.php
    * you don't create resources directly, they are created by a set of built-in functions which manage those things
    * there are **many** types of resources e.g.
        ```php
        mysql_connect() : mysql link resource
        ```
    * examples
        ```php
        get_resource_type(resource $handle) : string
        is_resource(resource $handle) : boolean
        ```
    * I'm guessing a `resource` is a reference to memory held by C
    * resources are mostly GCed but (CAREFUL NOW) persistent DB connections are not
10. null
    * represents a variable with no value
    * the only possible value is the constant NULL (note the caps)
    * variables are `null` if
        * they are assigned the constant `NULL`
        * they have been `unset()`
        * they have not been set to any value yet
    * relevant functions
        ```php
        is_null()
        unset()
        ```
    * Casting to `null` is deprecated


Pseudo types used in documentation but cannot be used in code

1. mixed
    * accepts multiple but not necessarily all types
2. number
    * = integer|float
3. callback
    * = alias for callable
4. array|object
    * = self explanatory
5. void
    * as a return type it means return value is useless
    * in a parameter list it means the function doesn't accept any parameters
6. `$...`
    * indicates an infinite number of argments

* variables
    * begin with `$` then a (single byte) letter or underscore
        * => PHP variable names cannot use mutli-byte utf-8
* variables are **always** assigned by value
    * when you assign the result of an expression to a variable the whole thing is _copied_ to the expression
* it has assign by reference available if you prepend the source variable with `&`
    * Only _named_ variables can be assigned by reference
* uninitialized variables get a default value depending on their type
    * boolean = FALSW
    * string = empty string
    * array = empty array
    * integer = 0
    * float = 0.0
* The first time that a variable is used in a scope, it's automatically created.
* PHP `isset()` won't tell you whether a variable is not set or is set but has NULL value
* Variable introspection
    ```php
    <?php
    // isset() returns true if $a is declared and not NULL (it won't tell you
    // the difference between undeclared and null)
    >>> isset($a);
    => false
    >>> $a = NULL;
    => null
    >>> isset($a);
    => false
    >>> $a = 3;
    => 3
    >>> isset($a);
    => true

    >>> isset($ar);
    => false
    >>> $ar = array();
    => []
    >>> isset($ar);
    => true

    // empty() returns true if variable is 1) unset 2) NULL or 3) is empty array
    >>> empty($b);
    => true
    >>> $b = 4;
    => 4
    >>> empty($b);
    => false
    >>> $b = array();
    => []
    >>> empty($b);
    => true
    >>> $b = NULL;
    => null
    >>> empty($b);
    => true

    // var_dump() outputs a string representation of the variable to STDOUT and returns NULL
    >>> var_dump($b);
	NULL
	=> null

    >>> $arr = array("name" => "Eoin");
    => [
        "name" => "Eoin",
    ]
    >>> var_dump($arr);
    array(1) {
    'name' =>
    string(4) "Eoin"
    }
    => null

    // gettype() returns string with name of the type of the argument
    >>> gettype($arr);
    => "array"


    ```

Namespaces

* PHP Namespaces provide a way in which to group related classes, interfaces, functions and constants.
* Only the following code cares about the namespace it was defined in
    1. classes (including abstract and trait)
    1. interfaces
    1. functions
    1. constants
* Namespace names are **not** case sensitive
* A file declaring a namespace must put that declaration at the very top of the file
*
* You can declare multiple namespaces in a file but this is strongly discouraged
* The same namespace can be declared in multiple files (allowing you to build your namespace of code objects from multiple files
* `\` is typically used to indicate levels within a namespace name
* The `use` keyword (when used outside of a closure context) allows **aliasing**
    * PHP can alias many kinds of things:
        1. alias a namespace name
        1. alias a class name
        1. alias a interface name
        1. alias a function name
        1. alias a constnat name
    * Full syntax: `use Some\Name\Space as MySpace;`
    * If you omit the `as ...` then it gets aliased as the last `\` chunk in the namespace name e.g.
        ```php
        <?php
        // these are equivalent
        use Some\Name\Space;
        use Some\Name\Space as Space;

        use Some\Name\Space\MyClassThing;

        use function Some\Name\Space\functionName;
        use constant Some\Name\Space\CONSTY;
        ```
    * `use` happens at compile time not runtime so you cannot have `use` inside blocks

```php
<?php

namespace Foo\Bar;

// stuff ...

// OR

namespace Foo\Bar {
    // stuff ...
}

// this puts your code in the "global" namespace
namespace {
    // stuff ...
}
```

## Debugging

```php
<?php

// old school
var_dump($data); // prints to stdout, returns null
die();

```

## Loading code

There are multiple autoloaders apparently

https://getcomposer.org/doc/01-basic-usage.md

```
require
require_once
```


## Questions

use
    pulls in the given symbol
    but how does it find it in files?
type hints
$this
namespace
how does loading code work in PHP?
