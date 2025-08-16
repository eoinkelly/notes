# Extending ruby

There are three Ways of extending ruby

1. Native extensions
    - write C code which includes `ruby.h` so you have access to the whole ruby
      interpreter
    - ++ you can do anything
    - -- you can do anything
    - -- you are super tightly bound to the ruby internals so your gem might
      break on new ruby versions
        - many C gems already do this which poses a big problem for ruby
          maintainers - see <https://www.youtube.com/watch?v=YLtjkP9bD_U>
1. Use FFI
    - see below
1. Use rubyinline
    - https://github.com/seattlerb/rubyinline
    - compiles C code embedded as a string in your ruby at runtime
    - doesn't seem like something I would use

## FFI

- <https://sourceware.org/libffi/>
- an open source lib that allows you to call compiled functions from interpreted
  code (where the function name, args are not known until runtime)
- allows your ruby/python etc. to be cross platform in the way it calls native
  code i.e. if `foolib` is available on macOS and linux the same ruby code can
  be used on both

Advantages of FFI over native extensions

1. You don't necessairly need to compile the C/Java code when you install the
   gem
    - ffi just needs access to a dynamic lib which can come from anywhere (be
      packaged with the gem or come from the system)
1. your ffi code doesn't depend on the internals of the ruby interpreter so can
   work across mri/jruby etc.
1. ffi gem can be used to bind to native C code or native Java code on the JVM

There are two popular ways to interact with libffi in ruby

2. fiddle
    - a `libffi` wrapper for ruby
    - ++ in standard lib
    - -- docs are pretty thin
1. ffi gem
    - ++ much better docs compared to fiddle
    - -- not in standard lib

## Candidate languages for native extensions

FFI will basically work with any language which can build a native shared
library

- C
    - -- no guard rails at all
- rust
    - ++ safer
    - has helix to make interfacing easier
        - -- helix is very incomplete as of 2016-11-29
- go
    - `gorb` is a wrapper thing for rust (akin to helix for rust)
        - still fairly immature as of 2016-11-29 - author describes it as a "a
          toy"
    - has been possible since go 1.5 when they added
      `go build -buildmode=c-shared`
    - but it might not be a good idea - see
      http://dev.mikamai.com/post/130986121064/writing-ruby-extensions-in-go-an-in-depth-review
        > Go has a performance problem when intercating with C and it’s by
        > design. ... calling C from Go has an aoverhead similar to calling ten
        > Go functions
    - it seems that go isn't particularly fast at calling raw C code itself so
      if your go code is calling C code on behalf of your ruby code then the
      whole thing can get mired in overhead and be slower than a pure ruby
      version!
    - QUESTION: does go GC make memory management hard when using it to build
      ruby extensions?
    - Aside: it looks like calling C code from go is messy and not particularly
      fast so Go isn't a great choice if you are going to call into small
