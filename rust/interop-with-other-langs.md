
# C

Rust has decent C interop but not c++ interop

# C++

Recommended way seems to be to wrap the C++ lib in C and then wrap that in Rust

The problem is that C++ has no uniformly standardised ABI

C++ doesn't even fully interoperate with itself :-)

< In C++, you can decide how moving values around works, whereas in Rust, the
compiler decides. All the moves for types like unique_ptr will possibly break
stuff.

An attempt: https://github.com/rust-qt/cpp_to_rust

Depending on the C++ compiler the compiled code may use

* different name mangling
* different calling convention
