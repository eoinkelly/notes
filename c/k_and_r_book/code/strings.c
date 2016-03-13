#include <stdio.h>
#include <wchar.h>

int main(int argc, char *argv[])
{
  char m1[5] = "hello";
  char m2[6] = "hello"; // 6 is correc size, allows for null byte
  char *m3 = "hello";

  printf("+++%s+++\n", m1);
  printf("+++%s+++\n", m2);
  printf("+++%s+++\n", m3);


  wchar_t *m4 = L"hello"; // create wide char literal
  printf("+++%S+++\n", m4); // print it with %S format specifier


  // \ causes compiler to ignore the following newline
  char *long_literal = "I am a \
                        multiline string\
                        literal";

  // compiler will automatically concat string literals beside each other
  char *concat = "hello, " "world";
  printf("hello there" " my name is eoin");



  return 0;
}
