#include <stdio.h>
#include <string.h>

int main()
{
  // create a **fixed** length array on the stack and set all bytes to 0
  int nums[10] = {};

  // We can't use designated initializer to 0 out an array because length is set
  // at runtime
  int len = 10;
  int nums_2[len];

  memset(nums_2, 0, len * sizeof(int));

  // Don't do this (slow and unnecessary):
  // int nums[10] = {};
  // for (int i = 0; i < 10; ++i) {
  //   nums[i] = 0;
  // }
}
