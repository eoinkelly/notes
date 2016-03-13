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
struct person *perpoi;
perpoi = &eoin;

printf("%s\n", (*perpoi).name); // dereference the pointer and call the member selection operator
printf("%s\n", perpoi->name);   // same thing (cleaner syntax)
```

* (nested) structures can be initialized with `{ ... }` same way (nested) arrays can.
* structures can be nested.

