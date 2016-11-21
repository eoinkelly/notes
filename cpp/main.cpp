#include <iostream>
#include <string>

using namespace std;

namespace Stuffy {
  void do_stuff()
  {
    cout << "doing stuff\n";
  }
}

namespace Foo {
  int x = 12;
}

namespace Bar {
  int x = 13;
}

void play_with_io()
{
  // TODO: what exactly are cin and cout? functions? cin can be passed as arg to getline()
  int raw_input;
  cout << "enter an integer: ";
  cin >> raw_input; // cin will cast the input to whatever type you give it
  // cout << "You entered: " + raw_input; // cannot concatenate because raw_input is not a string
  cout << "You entered: " << raw_input << "\n";
}

void sep_1(string title)
{
  cout << "********** " + title + " **************\n";
}

void sep_2(string title)
{
  cout << "***** " + title + " ******\n";
}

void play_with_strings()
{
  sep_1("String playground");

  sep_2("String concatenation");
  string greet = "string";
  string name = "concatenation is via +";
  string greeting = greet + " " + name; // + is string concatenation
  cout << greeting << endl;

  sep_2("String literals");
  // There are 5 ways of making a C string
  char plainString[] = "hello"; // Local encoding, whatever that may be TODO: how chosen?
  wchar_t wideString[] = L"hello"; // wide chars, may be UTF-16 or UTF-32 encoding depending on platform/compiler
  char utf8String[] = u8"hello"; // C++11 only
  char16_t utf16String[] = u"hello"; // UTF-16 encoding, C++11 only
  char32_t utf32String[] = U"hello"; // UTF-32 encoding, C++11 only

  cout << plainString << endl;
  cout << wideString << endl;
  cout << utf8String << endl;
  cout << utf16String << endl;
  cout << utf32String << endl;
}

void play_with_division()
{
  sep_1("Integer division returns an integer by truncation");
  int a = 5;
  int b = 2;
  int answer = a / b; // => 2
  cout << a << " / " << b << " = " << answer << endl;
}


using namespace Foo;
// using namespace Bar;

int main (int argc, char * const argv[])
{
  int notinit = 0;

  // my compiler does not warn about me using an uninitialized value unless you
  // run with -Wall
  cout << notinit
    << "\n"; // function calls can go across lines

  cout << "hello there\n"; // because we are 'using namespace std'
  std::cout << argc << "\n";
  std::cout << argv << "\n";
  printf("hello from C\n");
  printf("Program name is %s\n", argv[0]);
  Stuffy::do_stuff();

  // This variable demonstrates namespace clashing (it generates a compiler
  // error)
  cout << x << "\n"; // Uncomment `using namespace Bar` above and watch the fun
  play_with_io();
  play_with_strings();
  play_with_division();

  return 0; // optional, compiler will `return 0` if omitted
}
