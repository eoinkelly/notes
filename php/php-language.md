# PHP language

## General

* All PHP files must have opening tag `<%php`
    * if file is all PHP content (i.e. no interleaved HTML) then you should omit closing tag
* all statements (except last one in block) require `;` termination
* comments
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
* PHP is dynamically typed (types checked at runtime)
* recent versions (7+) have added more type declarations
* It supports explicit type casting e.g. `(string)`, `(float)` etc.
* It will do automatic type coercion if necessary
    * As ever, these rules are not always obvious or good
    * Use `declare(strict_types=1)` at top of each file to opt that file out of coercions

## Type declarations

> To enable strict mode, a single declare directive must be placed at the top of
> the file. This means that the strictness of typing for scalars is configured on
> a per-file basis. This directive not only affects the type declarations of
> parameters, but also a function's return type (see return type declarations,
> built-in PHP functions, and functions from loaded extensions.

> By default, PHP will coerce values of the wrong type into the expected scalar
> type declaration if possible. For example, a function that is given an int for a
> parameter that expects a string will get a variable of type string.

> It is possible to enable strict mode on a per-file basis. In strict mode, only
> a value corresponding exactly to the type declaration will be accepted,
> otherwise a TypeError will be thrown. The only exception to this rule is that an
> int value will pass a float type declaration.

```php
<?php
declare(strict_types=1)


```

## PHP has 10 primitive types

All type names are lowercase

1. boolean
    * use the (case insenstive) constants `TRUE` and `FALSE`
    * cast to boolean with `(bool)` or `(boolean)`
    * expressions are automatically **coerced** to boolean if passed to a control structure
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
        $a = 0123; // standard surprise octal
        $a = 0o123; // explicit octal
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
        // no parsing in here
        $a = 123;
        echo $a;
        Cloud be any data in here
        EOM;
        // note the semicolon after EOM
        ```
    * PHP strings are implemented as `(length, array of bytes)`
        * there is no `byte` type in PHP - strings play this role
    * Strings take their encoding from the encoding of the source PHP file
    * PHP strings are not unicode safe by default!
    * The `intl` and `mbstring` extensions contain functions which work correctly with multi-byte strings
    * There doesn't seem to be a clean way to iterate over a multibyte string. TODO
    * you can define the `__toString` method in your class to customise how it rendered as a string
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
    * boolean = FALSE
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

## Namespaces

* PHP Namespaces provide a way in which to group related classes, interfaces, functions and constants.
* Only the following code cares about the namespace it was defined in
    1. classes (including abstract and trait)
    1. interfaces
    1. functions
    1. constants
* Namespace names are **not** case sensitive
* PHP does not care what namespace names you use but devs follow PSR-4 convention
    * https://www.php-fig.org/psr/psr-4/
* PSR PHP Standards Recommendation
    * Created by PHP Framework Interop Group PHP-FIG
    * an attempt to make code more consistent between frameworks
* A file declaring a namespace must put that declaration at the very top of the file
* You can declare multiple namespaces in a file but this is strongly discouraged
* The same namespace can be declared in multiple files (allowing you to build your namespace of code objects from multiple files
* `\` is typically used to indicate levels within a namespace name
* The `use` keyword (when used outside of a closure context) allows **aliasing**
    * PHP can alias many kinds of things:
        1. alias a namespace name
        2. alias a class name
        3. alias a interface name
        4. alias a function name
        5. alias a constnat name
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

Old school: `include`

### include and include_once

* is an expression (returns a value)
* emits warning if it can't find the file
* behaves as if the code was typed in at that point in the file - gets whatever scope the `include` line is in
* include_once will handle re-includes

```php
<?php

// php searches the include_path, then . for this file
include 'file.php';

// explicit path - must start with / or . or .. to be recognised as explicit
include './path/to/file.php';
include 'path/to/file.php'; // won't be recognised as explicit


$myvar = include 'file.php'; // if the file has a `return` statemetn it will be returned
```

### require and require_once

* same as `include` but creates fatal error instead of warning if something goes wrong
* preferred

```php
<?php

require __DIR__.'path/to/file.php`
```

### handy magic constants

* https://www.php.net/manual/en/language.constants.magic.php
* resolved at compile time (regular constants are resolved at runtime)

```php
<?php
// __DIR__ is absolute path of current script's directory
// __FILE__ is absolute path of current script's file

```


### autoloaders

* Most PHP projects use a dynamic autoloader to avoid having many explicit `require` statements
* The composer autoloader is most popular and found in `vendor/autoload.php` in your project
* See https://getcomposer.org/doc/01-basic-usage.md


## Functional programming features

    Callables, which are things that can be called as functions, of which there are a few:
    PHP: Callbacks / Callables - Manual

    https://www.php.net/manual/en/language.types.callable.php

    Closures, a callable class with parameters and a scope bound to it:
    PHP: Closure - Manual

    https://www.php.net/manual/en/class.closure.php

    Anonymous functions, functions that are not defined globally, but are more like values that can be passed and assigned to variables:
    PHP: Anonymous functions - Manual

    https://www.php.net/manual/en/functions.anonymous.php

    Arrow functions, which are a shorthand way of creating anonymous functions:
    PHP: Arrow Functions - Manual

    https://www.php.net/manual/en/functions.arrow.php

    Finally, the magic __invoke method, which allows you to create callable objects:
    PHP: Magic Methods - Manual

    https://www.php.net/manual/en/language.oop5.magic.php#object.invoke

## Iteration

1. `for`
    * works like C
    ```php

    ```
1. `foreach`
    * works only on arrays and objects
    ```php
    // value is a copy
    foreach ($things as $value) {
        echo $value
    }
    unset($value); // required clean-up :-(

    // value is a reference
    foreach ($things as &$value) {
        $value = $value * 2;
    }
    ```
    * given a class instance (object) it will iterate through all the visible properties of that object
        * i.e. if iterating from outside the class you only see `public` properties, from inside you see everything
    ```php
    ```
    * you can implement the `Iterator` interface in your class to customise how it is iterated
        * https://www.php.net/manual/en/class.iterator.php

## Strings and encoding

* Your `php.ini` should have set
    ```php
    default_charset = "utf-8";
    ```
    * This seems to be set by default on modern installs

Many PHP functions are not unicode safe e.g. strlen is number of **bytes** in string, not characters. There are alternatives under the `iconv_*`, `grapheme_*` function name prefixes.

grapheme_ functions are par fo the Intl extension

```php
strlen() // count bytes
iconv_strlen() // count characters (I _think_ this is counting codepoints)
grapheme_strlen() // count graphemes
mb_strlen() // count ???
```

The `mbstring` extension provides a bunch of functions for working correctly with multi-byte encodings.

Frameworks provide their own String classes e.g.

* https://symfony.com/doc/current/components/string.html
* https://laravel.com/api/5.5/Illuminate/Support/Str.html

## Debugging

PHP defaults to writing debug output into the other output the script generates

```php
// php.ini

display_errors = 1 // should be 0 in production
error_reporting = E_ALL
//set to 32767 on my brew install
error_log path/to/error.log

```

how do i write to the server's stdout? can I?
how do i write to logs within frameworks?



```php
print_r($thing)
// ++ smart enough to invoke the iterator on arrays to see contents
// -- doesn't display boolean and NULL well
// -- will choke on recursive structures

var_dump($thing)
// ++ similar to print_r but handles booleans and NULL better
// ++ will stop after 3 levels on recursive structures


assert($a == 12, "a should be 12"); // deliberately triggering errors
```

## Errors Exceptions

Errors

* historically PHP had errors but not exceptoins
* Since PHP7 errors are reported by throwing `Error` exceptions.
    * the Error exceptoins **do not inherit from `Exception`!
    * `Throwable` is the common parent of `Error` and `Exception`
* errors can be suppressed by wrapping the expression in `@()`
    * You cannot otherwise recover from an error.
    * I'm not sure how useful this is since PHP7
* errors are procedural, exceptions are OO vibe
* you can set a global error handler function but it cannot handle all kinds of error
* exceptions are caught via the try-catch-finally syntax
* you can set a global exception handler to catch all exceptions you don't explicitly catch

## ::class syntax
what is this ::class syntax

     $app->make(Illuminate\Contracts\Console\Kernel::class);

> SomeClass::class will return the fully qualified name of SomeClass including
> the namespace. This feature was implemented in PHP 5.5.

## Questions
