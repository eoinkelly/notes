## Plan

* Finish my old C book

possible next steps
* do http://c.learncodethehardway.org/book/ex1.html
* do the K&R book

## Data types

* char | %c | ? bytes

* int           | %d    | ? bytes
* unsigned int  | %u    |
* long int      | %ld   |
* long (alias for `long int`)
* short int     | % ??  |
* short (alias for `short int`)

* float         | ? bytes | %f
* double        | %lf


```c
unsigned long x;
long y;
double d;
```


* C has no string variable type

### strings

* In C a string literal is a *pointer* e.g. evaluating `"hello"` will return a
  pointer.
* This means you can assign string literals to variables that point to `char`

```c
char *p; // create a pointer to a char
p = "hi there" // "hi there" evaluates to a memory address that is stored in p
```

* A string literal is an array of characters
* the name of an array is a pointer to the first element in it.

## Variable names

* Only first 31 chars matter
* [`a-zA-Z0-9_]` only. must not start with digit

## Arrays

* homogenous
* 0 based index
* must be sized when created
* defined by giving 1) type, 2) name, 3) number of elements

```c
int ages[11];
```

```c
// the size is often put in a perprocessor directive for convenience
#define SIZE 11

int ages[SIZE]
```


## Preprocessor

`define`

* Assocaiates symbolic names with a constant
* gets substituted in before compilation

# IO functions

## `scanf`

* Reads data from stdin

```
scanf( "%s", &ages[3] );
```

# getting memory address of a variable

* An `&` preceding a variable refers to the address of that variable
* `&foo` is the address in memory that the variable foo refers to.


# Storage classes

* storage class determines
    1. the part of memory where memory is allocated for the variable.
    2. how long the allocation continues to exist
1. `auto`
    * default storage class - if you omit a storage class when declaring a
      variable then `auto` is used.
    * `auto` variables are allocated when you enter a function and deallocated
      when you exit it.
    * also know as local variables
2. `static`
    * like `auto` they are scoped to the function they are declared in
    * but they are only allocated once and retain their value after the function
      has exited.
    * good for any state that is
        * should not be available to any other function
        * needs to be tracked across invocations of that function
        * they are sort of a global that is restricted to just that function.
    ```
    void foo() {
        static int x = 12;
    }
/   // x is still allocated but not available here
    ```
    Q: what does static outside a function do?
    * `static` set on a global variable _or_ function restricts access to it to
      just the file it was defined in. It reads as "make this global but only to
      this file".
        * it is a primitive form or access control
    * functions are _external_ storage class by default but can be made static.
3. `register`
    * supposodely stored in registers not in main memory
    * a way of suggesting to the compiler that we will need fast access to this
      variable - implementations do not garuantee they will do it.
    * they have the same scope as automatic variables (function local)
4. `extern`
    * used to tell the compiler that you want to use the _global_ copies of the
      named variables (PHP has a similar thing with its `global`)
    * they last for the lifetime of the program
    * functions in C are external by default (available to all source files)
    * there is one box in memory and all source files can use it.
    * `extern int foo; this keyword tells the compiler that "hey this integer
      called foo already has storage allocated for it by some other file so
      don't make a new box".
    * extern variables are compiled without the variable being resolved - it is
      resolved by the linking process.
    * to make a global
        * just declare the variable as normal in `file1.c` - it needs to be
          declared outside the scope of any function
        * reference it in any funcion in `file2.c` using `extern`
        * the variable name and type have to match obviously
    * the `extern` keyword is not used to make variables in the external area -
      you do that by delcaring them outside any functions.
       * external variables are created by the position of the declaration in
         the file.
       * all code following the declation can access that variable no matter
         what file it is in.
    * `extern` is used to tell the compiler that you want to reference global
      variables from within your function.

## Declaration vs definition vs instantiation

* Variables in C are _boxes_ in memory.
* The name is the identifier that your code will use to refer to the box.
* The type tells the compiler
    1. how big the box will be
    2. what kind of values can legally be in it.
* You can get access to the location of the box using `&` for _any_ variable


### Terminology

* Define _declare_
    * tell the compiler about the variable
    * tell the compiler the name and _type_ of the variable so it knows how much
      space to allocate and what values are legal.

* Define _define_
    * have the compiler allocate storage.
    * in the case of variables, the "definition" (or allocation of storage)
      happens at the same time as "declaration" for _automatic_ and _register_
      variables. They are different for _extern_
    * functions in C have a declaration and definition
    * you have to declare (but not define) a function before you use it.
        * there is a lot of duplication between the declaration and definition
          so presumably the declaration is to help the compiler.

* Define _instantiate_
    * declare + set a value on a variable (put a value in the box)
    * instantiation = the process of representing an abstract concept by an instance
    * instance := an example or single occurance of something
    * so i guess an instance is an example of something. `int x = 23` creates a
      box and puts something in it so we have a complete example of a variable.

Aside: C seems to be all about the human helping the compiler not the other way
round.

# Most complete form of variable delcaration

```
{storage-class} {type} {optional derefernce operator}{variable-name/;
```



# Type qualifiers (const, volatile)

* `const`
    * make this thing immutable
    * compiler should complain if you try to assign to this
    * they are usually initialized to get that first value in there

# Memory organisation

* Text (or Code) area
    * where the app instructions are put
* Stack area
    * a LIFO queue
    * memory is alloacted and deallocated automatically by the running code
    * function parameters, automatic (local) variables are stored here
* Heap
    * `external` and `static` variables are allocated here

# structs

* items in a struct are called _structure members_

```c
// create structure template (does not allocate any memory)
struct person
{
    int age;
    char *name;
    int scores[5];
}
```
* _structure members_ are not necessairly stored contigiously in memory (so you shouldn't do pointer math with them)
* First you create a structure template (but this does not allocate any memory)
* `.` is the _member selection operator_
* If you have a pointer to a struture you use `->` ?? operator to get at members.

```c
struct person
{
    char name[255];
    int age;
}

typedef struct person PERSON;

struct person eoin = { "Eoin Kelly", 35 };
struct person *pp;
perpoi = &eoin;

printf("%s\n", (*perpoi).name); // dereference the pointer and call the member selection operator
printf("%s\n", perpoi->name);   // same thing (cleaner syntax)
```

* (nested) structures can be initialized with `{ ... }` same way (nested) arrays can.
* structures can be nested.

# typedef

* lets you make a synonym for an _existing_ type.

```c
typedef char * STRING;

// These do the same thing
char *s = "hi";
STRING s = "hi";

// it does the right thing with multiple declarations separated by commas:
STING name, comment;

// NB #define would get this wrong
#define STRING char *
STRING a, b, c; // Looks ok but because define is simple text substitution it becomes ...
char *a, b, c; // a is a pointer to a char, b is a char, c is a char
```

* is often used with structures to avoid typing `struct my_thing` every time you
  need to create one.
* by convention alias is UPPERCASE.

# other stuff to revise

enum
unions

