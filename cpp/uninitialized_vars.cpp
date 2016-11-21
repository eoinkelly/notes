// Demonstrate what unintialized vars get set to in C/C++
#include <stdio.h>

int main(void)
{
  int a, b, c, d, e, f, g, h, i, j, k, l;

  printf("a = %d\n", a);
  printf("b = %d\n", b);
  printf("c = %d\n", c);
  printf("d = %d\n", d);
  printf("e = %d\n", e);
  printf("f = %d\n", f);
  printf("g = %d\n", g);
  printf("h = %d\n", h);
  printf("i = %d\n", i);
  printf("j = %d\n", j);
  printf("k = %d\n", k);
  printf("l = %d\n", l);
  return 0;
}

/*
In three runs on my laptop I got

a = different every time
b = always same, non-zero
c = different every time
d -> l = always 0

==============================
a = 136474678
b = 32767
c = 1551622864
d = 0
e = 0
f = 0
g = 0
h = 0
i = 0
j = 0
k = 0
l = 0
==============================
a = 117993526
b = 32767
c = 1545032400
d = 0
e = 0
f = 0
g = 0
h = 0
i = 0
j = 0
k = 0
l = 0
==============================
a = 386768950
b = 32767
c = 1449964240
d = 0
e = 0
f = 0
g = 0
h = 0
i = 0
j = 0
k = 0
l = 0
*/
