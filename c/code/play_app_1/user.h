#ifndef PLAY_APP_1_USER_H
#define PLAY_APP_1_USER_H

// defining a struct like this gives it two names:
//
//     typedef struct foo {
//       // ...
//     };
//     typedef struct foo foo_t;
//
// 1. 'struct foo'
// 2. 'foo_t'
//
// so we often perfer to create an anonymous struct and typedef it on the same
// line because that only creates one name.
//
typedef struct {
  char first_name[50];
  char last_name[50];
  int age;
  char occupation[100];
} user_t;

char* user_get_full_name(user_t*);

#endif // PLAY_APP_1_USER_H
