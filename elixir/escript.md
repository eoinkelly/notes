# Escript

http://www.erlang.org/doc/man/escript.html
http://elixir-lang.org/docs/v1.0/mix/Mix.Tasks.Escript.Build.html

* escript provides support for running short Erlang programs without having to
  compile them first and an easy way to retrieve the command line arguments.

* An escript can run on any machine that has Erlang installed
* by default does not require Elixir to be installed, as Elixir is embedded as part of the escript.
* `mix escript.build` task guarantees the project and its dependencies are
  compiled and packages them inside an escript.
* the escript file is a pkzip file with a short text header
* `unzip` command works on it
    * it unzips to a bunch of `elixir_*.beam` files and a few `*.app` files
    * `.app` files are text and contain what seems like a text representation
      of an elixir map that shows the metadata and dependencies of the app.

```
$ head ./some_escript_built_with_mix
#! /usr/bin/env escript
%%
%%! -escript main v2015_10_04_escript
<PKZIPPED DATA>
```

```
$ unzip my_app
$ cat my_app.app

{application,v2015_10_04,
             [{registered,[]},
              {description,"v2015_10_04"},
              {applications,[kernel,stdlib,elixir,logger]},
              {vsn,"0.0.1"},
              {modules,['Elixir.V2015_10_04']}]}.
```

