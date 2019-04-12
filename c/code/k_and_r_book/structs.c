#include <stdio.h>

/**
 * * A struct declaration actually reserves space if you supply variable names
 *   at the end - without variable names it just defines the shape of the type
 *
 * * C has no built-in operators for comparing structs - you have to write your
 *   own functions to do it.
 */

// a struct is a valid type declaration even without a tag
struct { char *name; int age; } human_1; // without tag
struct human { char *name; int age; } human_2; // with tag
// everything in the "struct...}" declares a type
// the shape is similar to
// int thing;


// everything in the "struct...}" declares a type
// this struct is tagged but does not reserve any storage
struct user {
  char *name;
  int age;
};

typedef struct person {
  char *name;
  int age;
} person;

// Structures can contain other structures
typedef struct point {
  int x;
  int y;
} point;

typedef struct rectangle {
  point p1;
  point p2;
} rectangle;

// ... you can even declare them inline (even though this example is bit rubbish)
struct rectangle_2 {
  struct point_2 { int x; int y; } p1;
  // compiler will warn if you are redeclaring a struct type so we use a
  // different name
  struct point_3 { int x; int y; } p2;
};

int main(int argc, char *argv[]) {
  // human_1 is a struct declared without a tag so you can't (at all???)
  // declare more instances of it - this could be a feature sometimes!
  human_1.name = "Eoin Kelly the human";
  human_1.age = 37;
  printf("%s\n", human_1.name);
  printf("%d\n", human_1.age);

  struct user eoin_1;
  eoin_1.name = "Eoin Kelly 1";
  eoin_1.age = 37;
  printf("%s\n", eoin_1.name);
  printf("%d\n", eoin_1.age);

  struct user eoin_2 = { "Eoin Kelly 2", 37 }; // using old-school init syntax
  printf("%s\n", eoin_2.name);
  printf("%d\n", eoin_2.age);

  // "struct person" is a type name
  struct person eoin_4 = { .name = "Eoin Kelly 3", .age = 37 };
  printf("%s\n", eoin_4.name);
  printf("%d\n", eoin_4.age);

  // "person" is a type name becaause of the typedef alias
  person eoin_3 = { .name = "Eoin Kelly 4", .age = 37 };
  printf("%s\n", eoin_3.name);
  printf("%d\n", eoin_3.age);

  // TODO: how does this work when I don't seem to have allocated any storage?
  struct person *eoin_p; // declare a pointer to a "struct person"
  (*eoin_p).name = "Eoin Kelly p1";
  (*eoin_p).age = 37;
  printf("%s\n", (*eoin_p).name);
  printf("%d\n", (*eoin_p).age);

  // declare automatic (stack) storage for a person and also a pointer
  struct person eoin_5, *eoin_p_5;
  eoin_p_5 = &eoin_5;
  eoin_p_5->name = "Eoin Kelly p_5";
  eoin_p_5->age = 37;
  printf("%s\n", eoin_p_5->name);
  printf("%d\n", eoin_p_5->age);

  return 0;
}
