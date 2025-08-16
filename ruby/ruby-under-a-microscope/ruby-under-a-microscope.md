# Ruby under a microscope

## Chapter 1

    Ruby-code --> [tokenize] --> [parse] --> [compile] --> Yarv-instructions
    Yarv-instructions --> ??? --> machine-code

QUESTION: what happens between yarv and machine code?

### Tokenize

- Convert characters in file to tokens
- Tokenization and parsing are separate processes which occur at the same time
    - the ruby parser runs the tokenizer whenever it needs a new token
- examples of tokens
    ```
    tINTEGER
    tIDENTIFIER
    keyword_do
    ```

#### Ruby parsing

- `parse.y`
    - contains the rules of the ruby grammar
    - ruby keywords are defined in `defs/keywords` which is fed into `gpref` (A
      C package which allows you to quickly lookup strings in a table)
    - the generated C code is in `lex.c` in a `rb_reserved_word` function
    - ruby does not use `Lex` with yacc and bison - ruby tokenization and
      parsing is written by hand.
    - contains a switch statement which does the tokenizing of ruby code:
        ```
        static enum yytokentype
        parser_yylex(struct parser_params *parser)
        ```
    - is a Bison grammar rule file
    - is where ruby's grammar is defined
    - ruby wants to share the parser between ruby and ripper
    - `parse.y` contains C snippets that are used by
        1. both Ruby and Ripper
        1. Ruby only
        1. Ripper only
    - these snippets are delimited by special comment chars

        ```
        TODO: this does not seem consistent

        /*%%%*/
        CODE_BUILTIN_TO_RUBY
        /*%

        /*%
        CODE_BUILTIN_TO_RIPPER
        %*/
        ```

- Bison is
    - a **parser generator** - it takes `parse.y` and generates `parse.c`
    - a newer version of YACC (yet another compiler compiler)
- Ruby uses a LALR parser
- `ruby -y`
    - will dump detailed parser state change info to console
    - `--yydebug` or `-y` turns on ruby debug mode
    - `-y` is not documented in `ruby --help` output but is in man page

#### LALR

- Look ahead, left, reversed rightmost derivation
    ```
    Look Ahead
    Left
    Reversed rightmost derivation
    ```
- Look Ahead
    - parser will peek ahead in the token stream to decide how to deal with the
      current token
        - QUESTION: how far?
- Left
    - parser consumes tokens from left to right
- Reversed rightmost derivation
    - parser takes a "bottom up" strategy
    - uses shift/reduce technique to find matching grammar rules
- the parser is a state machine
- each new token moves the machine from one state to another
- in ruby parser the states are assigned numbers

The parsing algorithm seems to be

    1. pull a token onto stack
    1. try to reduce the tokens on the stack
    1. if a reduction succeeds, try again. if not, continue
    1. ???
    Eoin: this is wrong/incomplete

The parser has to decide when to shift new tokens onto stack and when to reduce
the tokens already on there

```
$ ruby -y hello-world.rb

Starting parse
Entering state 0
Reducing stack by rule 1 (line 994):
lex_state: EXPR_NONE -> EXPR_BEG at line 995
cmdarg_stack(set): 0 at line 10340
-> $$ = nterm $@1 ()
Stack now 0
Entering state 2
lex_state: EXPR_BEG -> EXPR_CMDARG at line 7960
Reading a token: Next token is token tIDENTIFIER ()
Shifting token tIDENTIFIER ()
Entering state 35
Reading a token: Next token is token tSTRING_BEG ()
Reducing stack by rule 612 (line 4966):
   $1 = token tIDENTIFIER ()
-> $$ = nterm operation ()
Stack now 0 2
Entering state 118
Reducing stack by rule 61 (line 1484):
   $1 = nterm operation ()
-> $$ = nterm fcall ()
Stack now 0 2
Entering state 76
Next token is token tSTRING_BEG ()
Reducing stack by rule 262 (line 2490):
cmdarg_stack(push): 1 at line 2492
-> $$ = nterm @8 ()
Stack now 0 2 76
Entering state 230
Next token is token tSTRING_BEG ()
Shifting token tSTRING_BEG ()
Entering state 210
Reducing stack by rule 486 (line 4087):
-> $$ = nterm string_contents ()
Stack now 0 2 76 230 210
Entering state 415
Reading a token: Next token is token tSTRING_CONTENT ()
Shifting token tSTRING_CONTENT ()
Entering state 478
Reducing stack by rule 492 (line 4171):
   $1 = token tSTRING_CONTENT ()
-> $$ = nterm string_content ()
Stack now 0 2 76 230 210 415
Entering state 484
Reducing stack by rule 487 (line 4094):
   $1 = nterm string_contents ()
   $2 = nterm string_content ()
-> $$ = nterm string_contents ()
Stack now 0 2 76 230 210
Entering state 415
lex_state: EXPR_CMDARG -> EXPR_END at line 8014
Reading a token: Next token is token tSTRING_END ()
Shifting token tSTRING_END ()
Entering state 483
Reducing stack by rule 465 (line 3890):
   $1 = token tSTRING_BEG ()
... Eoin snipped output ...
```

You can also use ruby's `--dump parsetree` option to see how it parsed code.
This uses the real C names for things rather than the symbols you would see in
`Ripper.sexp` output

The following is the parsetree for

```ruby
puts "Hello, World!"
```

Notice that the args to puts are passed as a `NODE_ARRAY`

```
$ ruby --dump parsetree hello_en.rb
###########################################################
## Do NOT use this node dump for any purpose other than  ##
## debug and research.  Compatibility is not guaranteed. ##
###########################################################
#
# @ NODE_SCOPE (line: 1)
# +- nd_tbl: (empty)
# +- nd_args:
# |   (null node)
# +- nd_body:
#     @ NODE_PRELUDE (line: 1)
#     +- nd_head:
#     |   (null node)
#     +- nd_body:
#     |   @ NODE_FCALL (line: 1)
#     |   +- nd_mid: :puts
#     |   +- nd_args:
#     |       @ NODE_ARRAY (line: 1)
#     |       +- nd_alen: 1
#     |       +- nd_head:
#     |       |   @ NODE_STR (line: 1)
#     |       |   +- nd_lit: "Hello, World!"
#     |       +- nd_next:
#     |           (null node)
#     +- nd_compile_option:
#         +- coverage_enabled: true
```

TODO: dig into LALR properly - I don't have a clear understanding yet

UP TO END CHAP 1

## Chapter 2

- ruby compiles the AST generated by parsing into YARV bytecode
- ruby 1.8 did not have a compiler
- IMPORTANT: YARV bytecode is the final form that **your** code gets transformed
  into.
    - your code never directly becomes machine code
- Yarv is a stack oriented virtual machine
    - most instructions either push values onto the stack or
    - operate on some values form the top of the stack then replace them with a
      result

- internally ruby distinguishes between "function calls" and "method calls"
    - function calls assume the receiver is the current value of `self`
    - method calls have an explicit receiver
- ruby automatically creates a top-level "main" object for the script to run in.
    - All "function calls" at the top-level of a ruby script will use that
      object as their receiver
    - That object is pointed to by `self` at the top level of your script

    ```ruby
    puts "hi"
    # is same as
    Kernel.puts "hi"

    puts self # => "main"
    puts self.class # => "Object"

    # conceptually ruby's top level 'main' works a bit like this:
    class Object
        def main
            # code you write outside of any method is run within the context of an instance of the "main object"
            puts self # => "main"
            puts self.class # => "Object"
        end

        private
        # methods you define at top level go here
        def foo
            puts "hi"
        end

        def to_s
          "main"
        end
    end

    Main.new.main
    ```

Anatomy of the ruby AST

I pulled this together by observing the output of
`ruby --dump parsetree -e "some code"`

- NODE_SCOPE
    - contains
        1. nd_tbl (a table)
        1. nd_args (a list of args)
        1. nd_body
- NODE_PRELUDE
    - contains
        1. nd_head
        1. nd_body
        1. nd_compile_options
- NODE_FCALL
    - contains
        1. nd_mid (method id)
        1. nd_args (arguments)
- NODE_CALL
    - contains
        1. nd_mid (method id)
        1. nd_recv (the receiver)
        1. nd_args (arguments)
- NODE_ARRAY
    - contains
        - nd_alen (length)
        - nd_head (seems to be an array entry)
        - nd_next
- (null node)
    - contains nothing
    - represents an empty node
- NODE_LIT
    - contains
        - node_lit (a literal value)

```
ruby --dump parsetree -e "puts 2 + 2"
###########################################################
## Do NOT use this node dump for any purpose other than  ##
## debug and research.  Compatibility is not guaranteed. ##
###########################################################

# @ NODE_SCOPE (line: 1)
# +- nd_tbl: (empty)
# +- nd_args:
# |   (null node)
# +- nd_body:
#     @ NODE_PRELUDE (line: 1)
#     +- nd_head:
#     |   (null node)
#     +- nd_body:
#     |   @ NODE_FCALL (line: 1)
#     |   +- nd_mid: :puts
#     |   +- nd_args:
#     |       @ NODE_ARRAY (line: 1)
#     |       +- nd_alen: 1
#     |       +- nd_head:
#     |       |   @ NODE_CALL (line: 1)
#     |       |   +- nd_mid: :+
#     |       |   +- nd_recv:
#     |       |   |   @ NODE_LIT (line: 1)
#     |       |   |   +- nd_lit: 2
#     |       |   +- nd_args:
#     |       |       @ NODE_ARRAY (line: 1)
#     |       |       +- nd_alen: 1
#     |       |       +- nd_head:
#     |       |       |   @ NODE_LIT (line: 1)
#     |       |       |   +- nd_lit: 2
#     |       |       +- nd_next:
#     |       |           (null node)
#     |       +- nd_next:
#     |           (null node)
#     +- nd_compile_option:
#         +- coverage_enabled: false
```

http://graysoftinc.com/the-ruby-vm-interview/the-ruby-vm-episode-v

> YARV has two type instructions. First is primitive instruction. It's as
> written, primitive. Ruby code can be represented in these primitive
> instruction. Second is instructions for optimization. It's not needed to
> represent Ruby scripts, but they are added for optimization. Primitive
> instructions doesn't include \_ in their name (like putobject), and optimize
> instructions do (like opt_plus). This policy helps you if you want to see VM
> instructions. Initially, you need to read primitive instructions.

> YARV eases to implement such instructions with VM generator. You shouldn't
> write bothersome code such as stack manipulation. If you write VM instruction
> such as opt_plus in simple VM DSL, VM generator will translate it to C code.

> On YARV, bytecode and other information are represented as the
> VM::InstructionSequence class. I often use the name "ISeq" to point that
> class. ISeq object contains a bytecode sequence, a catch table (to retrieve
> exception and other global escape such as break), a local variable name table
> and others.

> ISeq object can be dumped in Ruby's primitive objects such as Array, Hash,
> Fixnum and so on. In the same way, ISeq can be built with such data with
> primitive objects. This means that you can built YARV bytecode without YARV
> compiler.
