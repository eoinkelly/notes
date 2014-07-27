#include <stdio.h>
#include <string.h>

#define EXIT_SUCCESS 0

//#include "other.c"

int localMagic = 567;

void spacer(const char *label)
{
    puts("**********************************");
    printf("    %s\n", label);
    puts("**********************************");
    // TODO: how to uppercase a string in C ???
}

void learningAboutStrings(void)
{
    spacer("strings");

    // * A string is an array of char values (in C strings ARE arrays)
    // * 'const char *s' is the type annotation for a string as an arg
    //      * this reads as
    //          "s is a pointer to an immutable character box in memory."
    //          "s is a pointer to an immutable character box in memory. If this char box is followed by other char boxes then s is the name of an array of char boxes"

    //            OR
    //          "s is the name of a character array (and also a pointer to the first element in it)"

    // the name of an array is a pointer to the first element of it so
    // a pointer to a character can be treated as the name of a character array where that char is the first one.

    // You can't tell by reading the type signature of a pointer whether it points to a single thing or an array of those things


    puts("I am a string");

    // Strings of known length:
    //      * create them as character array
    // store a string in a variable

    // create a pointer to a character
    // In C a string literal IS A POINTER!
    char *str1 = "I am a string known at compile time";

    // you cannot create a variable to hold a string of unknown length


    // p is a pointer to the memory address of the start of the char array
    // p is a variable pointer
    // p occupys a memory location itself so its value can be changed
    char *p = "hi";

    // s is a pointer to the memory address of the start of the char array
    // s is a constant pointer
    // s does NOT occupy a memory location so you cannot change its value
    char s[] = "hi";
}

void learningAboutConst(void)
{
    spacer("const");

    // const is a thing you can add to a variable declation to make it immutable
    int x;
    const int y;
    const int z = 14; // initialization is OK

    x = 12; // OK
    // y = 4; // compile error

    // You can choose to make either the pointer immutable or the thing it points to be immutable
    char c = 'a';


    const char *cp; // c is immutable through cp but the pointer itself can be changed
    char *const cp2 = &c; // the pointer is constant, not the thing it points to

    //*cp = 'x'; // compiler error because the character cannot be re-assigned through the pointer
    c = 'b'; // works because the character can be re-assigned through its name
    *cp2 = 'c'; // works because cp2 is a pointer that cannot be modified but what it is pointing at ca.n.

    char d = 'Y';
    //cp2 = &d; // comiler error because the pointer cannot be re-assigned

}
void learningAboutStructs(void)
{
    spacer("structs");
    // First we create a structure template
    struct person
    {
        int age;
        char name[];
    };

    // ... and then declare some person variables.
    struct person eoin;

   //eoin.name = "Eoin Kelly";
    eoin.age = 35;

    // Next we create an alias so we don't have to use 'struct person' all the time ...
    typedef struct person PERSON;

    // ... and declare a variable of type PERSON
    PERSON john;

    john.age = 33;
}

// sizeof(x) returns the size of the x in bytes
void showSizes() {
    printf("char: %lu\n", sizeof(char));

    printf("int: %lu\n", sizeof(int));
    printf("short: %lu\n", sizeof(short int));
    printf("long: %lu\n", sizeof(long int));
    printf("unsigned int: %lu\n", sizeof(unsigned int));
    printf("unsigned short int: %lu\n", sizeof(unsigned short));
    printf("unsigned long int: %lu\n", sizeof(unsigned long));

    printf("float: %lu\n", sizeof(float));
    printf("double: %lu\n", sizeof(double));



}

char *foo() {
    return "returned from foo\n";
}

void swapper(int *, int *); // function declaration

// Function definitions: option 1
//void swapper(a, b)
//int *a, *b;
//{
// this is not encouraged - it just exists to be compatible with the original design of C

// Option 2
//void swapper(int *a, int *b) {


// A function that takes two arguments by reference
void swapper(int *a, int *b)
{
    int temp = *a;
    *a = *b;
    *b = temp;
}

extern void helloFromOther(void);

void learningAboutStorageClasses(void)
{
    auto int x = 12;
    printf("current contents of the box x: %d\n", x);
    printf("location of the box named x: %p\n", &x);

    // I can mutate the contents of the box but I cannot change its location in memory - it is a
} // all auto variables are deallocated here

int main(int argc, const char * argv[]) {

    // ////////////////
    // external variables
//    extern int magicThing;
//    extern int localMagic; // optinal
//    printf("this is a magic thing from another file: %d\n", magicThing);
    printf("this is a magic thing from this file: %d\n", localMagic);
    helloFromOther();
    // ////////////////

    learningAboutStructs();

    learningAboutStrings();


    int xx = 12;

    learningAboutStorageClasses();

    printf("address of xx: %p\n", &xx);

    printf("hi everybody\n");
    printf("%s", foo());
    printf("Hello, World!\n");
    showSizes();

    // Arrays
    // ******

    // You don't have to explicitly size the array if you are initializing it.
    // These are equivalent:
    int things[] = { 1, 4, 5, 9 };
    int things2[4] = { 3, 4, 6, 0 };

    int things3[4] = { 1 }; // any missing values are set to 0

    int multi_dimensional_1[3][4] = { 1, 2, 3, 4, 5, 6 }; // this works!

    int multi_dimensional_2[3][4] = { 1, 2, 3,
                                    4, 5, 6 }; // slightly better!

    int multi_dimensional_3[3][4] = {
        { 1, 2, 3 },
        { 4, 5, 6 }
    }; // better still

    // # Pointers

    // {type-pointed-to} *{name-of-pointer}
    // char *foo; // read it backwards: foo is a pointer to a char

    // ## indirection operator `*`
    // `*` in C is the 'indirection operator'
    // * it is used in two different contexts:
    //  1. creating pointers
    //  2. getting the value from a pointer (dereferencing a pointer)
    //  These contexts are not related.
    // it is used to get the value of a variable, the address of which is stored in a pointer

    // For example
    //```c
    char c = 'a';
    char *cp = &c; // context 1: creating cp as a pointer to c
    printf("Value of c is: %c\n", *cp); // context 2: dereferencing cp to get c
    //```


    // # Strings

    // string concatenation is done wiht whitepace
    printf("hello" "there"); // works!
    // * this lets you split long strings over multiple lines

    // * a string is an array of chars
    // * the length of hte array is the number of chars plus \0 (which terminates the string)

    // These are some ways of creating a string:
    char *greet = { 'H', 'i', ' ', 'y', 'o', 'u' };

    //char *greet2[] = "Hi you"; // same as above (remember you can omit array length if you are initializing it)

    // like numerical arrays if you don't give it enough elements in intiailziation it will fill it with nulls
    //char *long_greet[10] = "hi"; // {'h', 'i', \0, \0, \0 etc. }

    // Note: no string type (strings are just arrays of char)

    greet[0];   // H
    *greet;     // H (array name = pointer to first element

    greet[1];        // i
    (*greet + 1);    // i (using pointer arithmetic. remember sizeof(char) == 1)


    // arrays are just constant pointers
    char a[3] = { 'a', 'b', 'c' }; // a is a pointer. it holds the address of the first byte in the array
    // you cannot change what a points to using pointer math (the indirection operator)
    // for example a = a + 1 will not increment the memory address by one

    // you can however do this using ordinary pointers
    char *p1 = a;
    // p1 = p1 + 1; // now p1 points at the second element in the a array

    // In C: a is exactly the same as &a[0]
    // strings are just arrays
    // => strings are just constant pointers too
    char *p = "some stuff";

    // ## String functions

    // int puts(const char *str)
    puts("I do same thing as in ruby");


    // # Functions

    // * C functions pass args by value by default
    // * you can pass in a reference to the arg instead at call time if you want to pass by reference
    // int a = 1;
    // int b = 2;
    // void foo(x, y) { ... }
    // void bar(&x, y) { ... }
    // foo(a, b); // pass in a copy of a and b, a,b remain the same
    // bar(&a, b); // pass a reference to a and a copy of b


    int first = 12;
    int second = 56;
    swapper(&first, &second);
    printf("first: %d\n", first);
    printf("second: %d\n", second);

    return EXIT_SUCCESS;
}
