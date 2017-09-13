#include <stdio.h>
#include <stdint.h>

void test_diff_between_char_types();

int main(int argc, char *argv[])
{
  // Nothingness in C
  //
  // * There are two places where C deals with nothingness
  //  1. "null pointer" is a special pointer value which indicates an missing value.
  //      * Its exact implementation is platform dependent but is usually
  //        either just integer 0 or integer 0 cast to a void pointer (so that it
  //        can be assigned to any pointer type).
  //      * The macro `NULL` is defined by `stddef.h` to help test for this value
  //  2. "null character" aka a character with all its bits set to 0
  //      * used to indicate the end of a string
  //
  char nul_char_by_nul_char = '\0';
  char nul_char_by_integer_zero = 0;
  if (nul_char_by_integer_zero == nul_char_by_nul_char) {
    printf("the nul character and int 0 are equivalent\n");
  } else {
    printf("the nul character and int 0 are NOT equivalent\n");
  }

  printf("sizeof(int): %ld\n", sizeof(int)); // => 4
  printf("sizeof('a'): %ld\n", sizeof('a')); // => 4 - type of 'a' is int
  printf("sizeof(char): %ld\n", sizeof(char)); // => 1
  printf("sizeof(unsigned char): %ld\n", sizeof(unsigned char)); // => 1
  // * C used integer 0 for the concept of nothingness


  // * The C `char` datatype is (confusingly) an integer type. It should probably be called `byte`
  //    * it is the smallest integer type
  //    * C does not have a dedicated character type!
  // * You can assign an integer literal or a character literal to a `char` type.
  // * character literals just a shorthand for looking up the integer value in the ASCII table
  //
  // The C typesystem has three "very small int" types:
  //
  //   1. char
  //   2. unsigned char
  //   3. signed char
  //
  // * These are three distinct types from the C compiler's pov
  // * `char` will be implemented as either 2. or 3. depending on the compiler
  //   - you cannot depend on this behaviour.
  //
  // * C stdlib functions expect a `char` type, not the others
  // * There are envs where you might want to do math with `char` type because
  //   you need to do it on 8bit values and C does not have a `byte` type.
  // * `char` is defined as being big enough to hold ASCII. You should NOT use
  //   it to hold any **text** outside the ascii range
  //
  // > in C you can use a char to do math, when it will make a difference.
  // > In fact, when working in constrained memory environments, like embedded
  // > 8 bit applications a char will often be used to do math, and then it makes
  // > a big difference. This is because there is no byte type by default in C.
  //
  // * Use `signed char` or `unsigned char` as a way of indicating that you
  //   intend to treat this data as 1 byte numbers
  // * Use `char` when you want to treat the data as a string i.e. you only
  //   want to store and retrieve it - you will not do arithmetic on it.
  //
  uint8_t maybe = 33; // uint8_t is defined in stdint.h

  char x = 200;
  printf("x is %c\n", x);
  if (x == 200) {
    printf("Still 200\n");
  } else {
    printf("Changed\n");
  }


  char char_by_char_literal = 'a';
  char char_by_integer = 97;
  if (char_by_char_literal == char_by_integer) {
    printf("character literals in C are just short hand for setting the ASCII code\n");
  } else {
    printf("character literals in C are different than integers\n");
  }


  // Strings in C
  //
  // * All data in C programs are just values
  // * A string in C is just an array of 'char' values
  // * QUESTION: what is a char value? is it always "1 byte". I presume it could differ across architectures
  //    * what does C standard promise about char values?
  // * By convention C strings end in a null character '\0`
  //    * C string APIs expect this
  //    * C automatically appends a nul character to any string literals you enter in code
  //
  // String literals
  //
  // * You can put string literals in your C code
  // * They are treated as shorthand by the compiler for
  //    1. entering a nul terminated array of characters


  // * Arrays of chars are NOT automatically null terminated
  // * This array is allocated on the stack
  // * s3 is an array of 6 elements
  char s3[] = {'h', 'e', 'l', 'l', 'o', '\0'};

  // * C string literals are automatically null terminated
  // * This array is allocated on the stack
  // * s2 is an array of 6 elements (including null termination char)
  // * s2 is just a handy shorthand for s3
  char s2[] = "hello";

  // s2 is exactly the same as s3
  //
  // so waht happens if I setup a string literal liek this?
  // * The literal string is copied to the read-only section of memory in the process
  // * s is a pointer to a char
  // * s is also a pointer to an array of 6 items (including null termination)
  char *s = "hello";

  /* printf("%s ---\n", s); */
  /*  */
  /* char c = 'a'; */
  /* char * cp = &c; */
  /* // %s expects a 'char *' */
  /* printf("%s ---\n", cp); */

  // %s assumes you will give it a null terminated thing

  /*  */
  /* char * cp_normal = &c; */
  /* printf("%s\n", cp_normal); */
  /*  */
  /* const char * cp_pointer_to_constant_char = &c */
  test_diff_between_char_types();

  return 0;
}

void test_diff_between_char_types() {
    printf("+++++++++++++++++++++++++\n");
    char a = 'A';
    char b = 0xFF;
    signed char sa = 'A';
    signed char sb = 0xFF;
    unsigned char ua = 'A';
    unsigned char ub = 0xFF;
    printf("a > b: %s\n", a > b ? "true" : "false");
    printf("sa > sb: %s\n", sa > sb ? "true" : "false");
    printf("ua > ub: %s\n", ua > ub ? "true" : "false");
}
