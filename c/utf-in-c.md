# UTF in C

It seems to be that using UTF-8 in normal char type as the internal
representation of strings in your program is recommended. This is recommended
over wchar_t and friends - see http://utf8everywhere.org/

ICU seems to be the most recommended library if you need to do complicated
unicode things (not just getting and storing text) http://site.icu-project.org/

Overview of reasons

- UTF-8 is endianness independent unlike UTF-16 which has separate flavours for
  each endianness

# wchar_t

- wide character type
- `#include <wchar.h>
- format specifier is `%ls`
    - using this format specifier causes `printf` and `scanf` to correctly
      process wide chars
- `wcslen()` to get length of widechar string
- Text literals prefixed by `L` are treated as wide char eg.
  `wchar * x = L"this is wchar"`
- wchar_t does not have a fixed size across compilers - it was introduced at a
  time where 16bit chars were seen as being enough

- C1X
    - added two new fixed size types
        - wchar16_t
        - wchar32_t
    - and three new literal specifiers
        - `u` = 16 bit char literal
        - `U` = 32 bit char literal
        - `u8` = UTF-8 encoded 8 bit literal

```c
char plain[] = "plain string";
wchar_t wide[] = L"wide char string, might be 16 or 32 bit chars"
char utf8_str[] = u8"I am single btye string but UTF-8 literal"
wchar16_t utf_16[] = u"I am UTF-16 encoded literal"
wchar32_t utf_32[] = u"I am UTF-32 encoded literal"
```
