## PCH files

* Called the `prefix header`
* It is *automatically included in every source file* in the project without the
* use of compiler directives.
* They are _usually_ pre-compiled and cached to speed up build times.
    * They only have to be parsed once by the compiler
* they _can_ be used for project wide `#define` but those are a bit of a code
  smell.
* You should only `#include` headers in here that change rarely - otherwise the
  caching will be a net negative
* Downsides
    * they create a hidden dependency between your Foo.m|h files and the prefix
      header which means your source files can't be shared with other projects
      without it.
* Xcode 6 does not seem to make a PCH for new projects

* Opinion: http://qualitycoding.org/precompiled-headers/

