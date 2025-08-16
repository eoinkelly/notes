# Ruby typing as of 2023-02-12

- `rbs`
    - installed by default in Ruby 3+
- `typeprof`
    - generate .rbs files from .rb files
    - https://github.com/ruby/typeprof
- `rbs_rails`
    - https://github.com/pocke/rbs_rails
    - generates rbs from your rails app code
- `steep`
    - the actual type checker
    - https://github.com/soutaro/steep

Why separate file?

> most of the Ruby standard library is implemented in C for performance (e.g.,
> all Array and Hash functions, and many others). There must be a way to ascribe
> types to these internally defined classes and methods.

Sorbet

- https://sorbet.org/blog/2020/07/30/ruby-3-rbs-sorbet
    - can/will use rbs types as well as it's own syntax
    - sorbet has both an inline syntax and a separate file syntax (.rbi files)
