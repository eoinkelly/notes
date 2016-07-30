# Docs:
# http://elixir-lang.org/docs/stable/elixir/Kernel.SpecialForms.html

# __ENV__
# #######
#
# * A macro
# * Returns current environment info as an instance of %Macro.Env
# Contains
#     * current filename __ENV__.file
#     * current function
#     * available aliases
#     * list of macros, organised by module
#     * imported functions
#     * current line within current function
#
# Use `h Macro.Env` to get info on its structure
IO.inspect __ENV__

# iex(146)> __ENV__
# %Macro.Env{aliases: [], context: nil, context_modules: [], export_vars: nil,
#  file: "iex", function: nil,
#  functions: [{IEx.Helpers,
#    [c: 1, c: 2, cd: 1, clear: 0, flush: 0, h: 0, i: 1, l: 1, ls: 0, ls: 1,
#     nl: 1, nl: 2, pid: 1, pid: 3, pwd: 0, r: 1, recompile: 0, respawn: 0, v: 0,
#     v: 1]},
#   {Kernel,
#    [!=: 2, !==: 2, *: 2, +: 1, +: 2, ++: 2, -: 1, -: 2, --: 2, /: 2, <: 2,
#     <=: 2, ==: 2, ===: 2, =~: 2, >: 2, >=: 2, abs: 1, apply: 2, apply: 3,
#     binary_part: 3, bit_size: 1, byte_size: 1, div: 2, elem: 2, exit: 1,
#     function_exported?: 3, get_and_update_in: 3, get_in: 2, hd: 1, inspect: 1,
#     inspect: 2, is_atom: 1, is_binary: 1, is_bitstring: 1, is_boolean: 1,
#     is_float: 1, is_function: 1, is_function: 2, is_integer: 1, is_list: 1,
#     ...]}], lexical_tracker: nil, line: 146, macro_aliases: [],
#  macros: [{IEx.Helpers,
#    [b: 1, h: 1, import_file: 1, import_file: 2, s: 1, t: 1]},
#   {Kernel,
#    [!: 1, &&: 2, ..: 2, <>: 2, @: 1, alias!: 1, and: 2, binding: 0, binding: 1,
#     def: 1, def: 2, defdelegate: 2, defexception: 1, defimpl: 2, defimpl: 3,
#     defmacro: 1, defmacro: 2, defmacrop: 1, defmacrop: 2, defmodule: 2,
#     defoverridable: 1, defp: 1, defp: 2, defprotocol: 2, defstruct: 1,
#     destructure: 2, get_and_update_in: 2, if: 2, in: 2, is_nil: 1, match?: 2,
#     or: 2, pop_in: 1, put_in: 2, raise: 1, raise: 2, reraise: 2, ...]}],
#  module: nil, requires: [IEx.Helpers, Kernel, Kernel.Typespec],
#  vars: [ast: nil, simple_ast: nil, things: nil]}


# __DIR__
# #######
#
# a macro that returns the *absolute path* of the directory of the *current file*

IO.inspect __DIR__

# is a shortcut for

Path.dirname(__ENV__.file)


defmodule Envy do
  def dump_env do
    IO.inspect __ENV__
  end
end
