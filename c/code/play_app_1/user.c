#include "user.h"
#include <stdio.h>

// options
//
// 1. put it on the heap and require caller to call free()
//    -- caller must do cleanup
//    -- caller cannot decide that this can be stack allocated
//
// 2. require caller to allocate the buffer for us
//    ?? how does caller know how much to allocate?
//    ++ caller can choose where the memory should live
//    -- caller has to do extra work to get the answer it wants
//
char* user_get_full_name(user_t* user)
{
  char* buf;
  int ret = asprintf(&buf, "%s %s", user->first_name, user->last_name);
  if (ret == -1) {
    printf(
        "asprintf could not allocate enough memory on the heap for the "
        "string\n");
  }

  return buf; // caller must call free() on this
}
