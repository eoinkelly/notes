# Types

Sources

* https://hexdocs.pm/elixir/typespecs.html#types-and-their-syntax

## Dialyzer

* is a static analysis tool for `.beam` files (not .erl or .ex)
* can be useful even without adding typings (also called "type specs" or "type hints") to the code
* use `dialyxer` to integrate it with a mix project
* is based on the "Success Typings" paper http://user.it.uu.se/~kostis/Papers/succ_types.pdf
* Persistent lookup table (PLT)
    * the cached output of dialyzers analysis
    * saves time so you don't have to re-run on files which have not changed
* dialyxir
    * stores PLT files under `$MIX_HOME`
        * stores one PLT for each erlang/OTP version
        * stores one PLT for each erlang/OTP verions + Elixir version
        * stores a project specific PLT under `_build/env`
    * the core `.plt` files are just copied to your project if they already exist

what is a PLT?

```
# First install dialyxir

$ mix dialyzer
# The first run of dialyzer on your machine will be **very slow**

$ mix dialyzer.explain # list info about a all known warnings
$ mix dialyzer.explain some_warning_name # list info about a single warning
```
