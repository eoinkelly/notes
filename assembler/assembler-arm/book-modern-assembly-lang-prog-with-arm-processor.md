# Modern assembly language programming with the ARM processor

## Chapter 1

* a single line of assembly _usually_ gets translated into a single machine instruction.
    * What are counter examples to this?
* Assembly `MOV` is actually a copy operation
* ARM originally stood for _Acorn Risc Machine_ but was rebranded _Advanced Risc Machine_
* More ARM processors are produced annually than any other processor design combined
* CPUs have a "default word size" - usually 32bit or 64 bit on modern CPUs
* Building circuits with two states is much easier than building circuits with more so computers use base 2
* When manual conversion between bases it is easiest to convert into base-10 as an intermediate step e.g. `base-N --> base-10 --> base-M`

### Number representation

#### Unsigned integers

* can be represented as-is in base 2

#### Signed integers

There are three main ways of representing signed integers

1. sign magnitude
    * the MSB (most significant bit) represents the sign (1 => negative, 0 => positive)
    * other bits represent the magnitude of the number
    * used to represent the mantissa in floating point numbers
    * ++ easy for humans to read and understand
    * -- addition and subtraction logic is more complex
    * -- has two representations for 0 (0 and -0)
1. Excess-N
    * the number stored is N greater than the actual value
    * used to represent the exponent in floating point numbers
    * the "bias" added is usually `2^(N-1) - 1` e.g. for N = 8, bias = 127
1. two's compliment
    * the "complement" is the amount that must be added to something to make it whole
    * the first digit is used to represent whether the number is positive or negative
        * this is subtly different to how it works in _sign magnitude_
        * number is negative if `first_digit > radix / 2`
        * Aside: two's complement is not a more efficeint way to store numbers
          (it is more efficient for doing computations with them)
    * the other digits represent the magnitude
        * the magnitude of a number can be found by taking its "radix complement"
            * formula is `radix_complement = base^num_digits_in_original_number - original_number`
            * finding the radix complement involves a subtraction (which is slow in hardware)
            * but we can find the "diminished radix complement" using a "complement table"
                * `(base^num_digits_in_original_number - 1) - original_number`
                * the _complement table_ for base 2 is just a bit flip so is fast in hardware
                * we can get from the _diminished radix compliment_ to the _radix complement_ by adding 1
    * to get the two's complement
        1. flip all the bits (to get the diminished radix complement)
        1. add 1 (to get to the radix complement)
    * ++ hardware is simplified because you don't have to build a specialized subtractor circuit
    * ++ is the most common way to represent signed numbers in computers
    * -- harder for humans to do in their head
    * complement notation is most useful in base 2 (binary) numbers

```plain

a: 123
a_comp: 10^3 - 123 = 877

b: -450
b_comp: 10^3 - -450 = 1450

# Task: calculate a + b

result_comp = a_comp + b_comp
            = 877 + 1450
            = 2327

to get from complement result to the original result
* first digit of complement result becomes the sign.
    * digit is 2 which is less than (base / 2) so answer is negative
* the other digits are the magnitude
* => the answer is -327

# Task: calculate x - y

* assume that x and y are both positive and y <= x i.e. we are saying that y is
  the smaller of the two numbers if they are not equal

x = 66
y = 23
n = number of digits (in this case 2)
base = base/radix of number

result = x - y
       = x + base^n - y - base^n # add and subtract base^n
       = x + C(y) - base^n

we know that the result of X + C(y) will be in the range base^n <--> 2*base^n
    how???
=> we can assume that x + C(y) will have a 1 in the n+1 digit
=> if we remove the n+1 digit it is the same as if we subtract base^n

result = x + C(y)
       = 66 + 77
       = 143

first digit is the n+1 digit which we remove to subtract base^n from the number
=> answer is 43
```

### Character representation

* ASCII uses only 7 bits to maximize perf on slow connections (it was a 10% speed-up over 8-bit)
* Assembler uses ASCII NUL (0x00) to represent the end of strings in memory aka "the null terminated string"
* ISO 8559 family of standards all extend ASCII to 8 bits
    * They are incompatible with each other e.g. ISO-8559-1 (Latin1) will map different glyphs to values 0x7F to 0xFF than ISO-8559-9 (Latin 5 Turkish) will
* ISO/IEC 10646 Universal charaacter set solves this
    * defines code points (numbers) for almost all human languages
    * code points written as `U+XXXXX`
* Unicode extends ISO 10646 with language specific features
* There are a number of encodings available for codepoints from 10646 e.g.
    * UTF-8
        * variable length encoding
        * uses the upper bits of each byte to identify whether it is the start of a new character of the continuiation of one
    * UTF-16
    * UTF-32
        * use 4 bytes for each character
        * ++ simple
        * -- wastes space
* maximum UCS code point is 0x10FFFF

```plain
UTF-8 encoding
                                    byte 1     byte 2     byte 3     byte 4     byte 5
 7 bit char:  U+0000 --> U+007F     0XXX XXXX
11 bit char:  U+0080 --> U+07FF     110X XXXX  10XX XXXX  10XX XXXX
16 bit char:  U+0800 --> U+FFFF     1110 XXXX  10XX XXXX  10XX XXXX  10XX XXXX
21 bit char:  U+10000 --> U+10FFFF  1111 0XXX  10XX XXXX  10XX XXXX  10XX XXXX  10XX XXXX
```

### Memory layout

* CPU is "byte addressable" if it can address a single byte of memory
* CPU is "word addressable" if it can address a single word of memory
* ARM CPUs are byte addressable
    * their word size is typically 32bit
* ARM CPU can be configured to be big or little endian
    * Linux makes it little endian by default

Storage is allocated in one of three ways in high-level languages

1. Statically
    * stored in the `.data` (initialized data) and `.bss` (uninitialized data) sections of the binary
    * The C compiler decides whether to put static variables in `.data` or `.bss`
        * the BSS segment typically includes all uninitialized objects (both
          variables and constants) declared at file scope (i.e., outside any
          function) as well as uninitialized static local variables (local
          variables declared with the static keyword)
1. Dynamically
    * allocated on the heap with `malloc`, `new` etc.
1. Automatically
    * stored on the stack

In C

* statically-allocated objects without an explicit initializer are initialized
  to zero (for arithmetic types) or a null pointer (for pointer types).
* Implementations of C **typically** represent zero values and null pointer values
  using a bit pattern consisting solely of zero-valued bits (though this is not
  required by the C standard).
* Hence, the BSS segment typically includes all uninitialized objects (both
  variables and constants) declared at file scope (i.e., outside any function)
  as well as uninitialized static local variables (local variables declared
  with the static keyword);
* static local constants must be initialized at declaration, however, as they
  do not have a separate declaration, and thus are typically not in the BSS
    section, though they may be implicitly or explicitly initialized to zero.

### Chapter 1 exercises

#### 1

```plain
1101 1101    0xDD
0010 0010    0x22
0010 0011    0x23
```

## Chapter 2

An assembly program contains four things

1. labels
    * always end with `:` e.g. `main:`
    * get converted into the value of the address counter of the currently open section by the assembler
2. comments
    * `/* multi line like C */`
    * `@ single line like this`
    * `// single line also like this iff file ends in .S`
3. assembly instructions
    * _most_ cause the CPU to perform one operation
    * 3 categories of instruction
        1. move data
        2. perform some computation e.g. addition, subtraction
        3. perform comparisons and control which part of the assembly to execute next
4. assembler directives
    * always begin with `.` e.g. `.globl`

each line of code is organised into four columns

1. label (optional)
2. assembler directive or assembly instruction
3. operands to the directive or assembly instruction (optional)
4. comment (optional)

### Directives

#### Sections

* section directives "select" a section i.e. they indicate to the assembler which section the following lines should go in
* the form is `.section_name subsection_number`
* `subsection_number` defaults to 0 if omitted
* each section has an "address counter" which is what labels mark positions on
* common sections
    * `data`
        * global variables and labeled constants
    * `bss`
        * `bss` stands for _block started by symbol_
        * reserved data storage areas
        * initialized to zero by the executable loader
    * `text`
        * executable instructions
        * constant data
* custom sections are possible but the linker must know about them

```asm
.data // .data 0

.bss // .bss 0

.text // .text 0

.data 1 // open subsection 1 within data section
```

#### Allocating space in the data section

* each allocation directive can take multiple operands separated by comma
* each allocation directive usually has a label to identify it later
* if you don't pass any expressions to these directives they don't do anything
* assembler supports the following types of data
    * single byte type
        * `.byte`
            * data can be a number 0-255 or (apparently) a single character in single quotes
    * integer types
        * `.hword` (alias `.short`)
            * 16 bit storage
        * `.word` (alias `.long`)
            * 32 bit storage
    * floating point types
        * `.float` (alias `.single`)
        * `.double`
    * string
        * `.ascii` creates a string
        * `.asciz` creates a null terminated string
* labels are scoped to a single file unless you explicitly make them global with `.globl`
    * this is the opposite to C (which is global by default unless you use `static` to scope them to current file

```asm
byties:     .byte   'A', 'B', 0, 230
num_1:      .hword  333
num_2:      .word   5555
greeting:   .asciz  "hello there"
long_greet: .ascii  "hello there\n"
            .ascii  "my name is eoin\n"
            .asciz  "and this is some code\n"
pi:         .double 3.1459
```

```plain
ARM GAS  data.S       page 1


 GNU assembler version 2.25 (arm-linux-gnueabihf)
   using BFD version (GNU Binutils for Raspbian) 2.25.
 options passed  : -aghmls=data.lst
 input file      : data.S
 output file     : a.out
 target          : arm-unknown-linux-gnueabihf
 time stamp      : 2016-06-28T21:31:20.000+1200


ARM GAS  data.S       page 2


   1                .data
   2 0000 414200E6   byties:     .byte   'A', 'B', 0, 230
   3 0004 4D01       num_1:      .hword  333
   4 0006 B3150000   num_2:      .word   5555
   5 000a 68656C6C   greeting:   .asciz  "hello there"
   5      6F207468
   5      65726500
   6 0016 68656C6C   long_greet: .ascii  "hello there\n"
   6      6F207468
   6      6572650A
   7 0022 6D79206E               .ascii  "my name is eoin\n"
   7      616D6520
   7      69732065
   7      6F696E0A
   8 0032 616E6420               .asciz  "and this is some code\n"
   8      74686973
   8      20697320
   8      736F6D65
   8      20636F64
   9 0049 26E4839E   pi:         .double 3.1459
   9      CD2A0940
  10

ARM GAS  data.S       page 3


DEFINED SYMBOLS
              data.S:2      .data:00000000 byties
              data.S:3      .data:00000004 num_1
              data.S:4      .data:00000006 num_2
              data.S:5      .data:0000000a greeting
              data.S:6      .data:00000016 long_greet
              data.S:9      .data:00000049 pi

NO UNDEFINED SYMBOLS
```

Moving a word between memory and CPU is much slower if the word is not aligned on a 4 byte boundary

* words (32 bit) should be stored on 4 byte boundaries (two least significant bits are 0)
* half-wards (16 bit) should be stored on 2 byte boundaries (least significan bit is 0)

There are six directives that control alignment

1. `.align <num-low-order-bits>, <fill-value>, <max-num-bytes-to-skip>`
    * pad the location counter in the current section to a particular boundary
    * fill-value defaults to 0 if omitted
    * max-num-bytes-to-skip can be omitted
1. `.balign <byte-multiple-for-alignment>, <fill-value>, <max-bytes-to-skip>`
    * pad the location counter to be a multiple of the `<byte-multiple-for-alignment>``
    * if the location counter is already on the boundary then nothing is added
1. `.balignw`
    * same as `balign` but treats the `<fill-value>` as a 2 byte word
1. `.balignl`
    * same as `balign` but treats the `<fill-value>` as a 4 byte word
1. `.skip <size> <fill>`
    * if `<fill>` is omitted it is assumed to be 0
    * useful for declaraing large arrays in the `.bss` section
    * it is good practice to always tweak aligment after reserving byte or half-word data
1. `.space`
    * alias for `.skip`


Directives for manipulating symbols

* `.equ <symbol>, <expression>` (alias `.set <symbol>, <expression>`)
    * lets you assign a label to the result of an expression
    * similar to `#define` in C
* `.equiv <symbol>, <expression>`
    * same as `.equ` and `.set` except the assembler will throw an error if you try to redefine a symbol
* `.globl <symbol>` (alias `.global <symbol>`)
    * makes the symbol available to the linker
* `.comm <symbol> <length>`
    * declares the symbol to be a "common symbol"
    * the assembler will find all definitions of this symbol and allocate only one chunk of storage (at whatever the longest declared `<length>` is)
    * multiple declarations of the symbol are merged into a single declaration

Current value of the address counter is denoted by `.` in expressions

```asm
ary: .word 6,9,9,0,7
.equ arrlen, (. - ary)/4
```

Conditional assembly directives

* `.if <expression>`
* `.else`
    * works for `.if` and `.ifdef`
* `.endif`
* `.ifdef <symbol>`
* `.ifndef <symbol>`

* `.include "other-file.s"`
    * the code is assembled as if it was pasted right in at that spot
    * the -I command line param can add paths for the assembler to search for included files

Macros

* `.macro`
    * opens a macro
* `.exitm`
    * return early from the macro
* `.endm`
    * signal end of the macro
* macro arguments
    * can have default values
    * arguments are available prefixed by `\` within the macro e.g. `p1` becomes `\p1`
* invoking a macro
    * pass args either positionally or with keywords
* macros can call themselves recursively
    * lets you emulate looping constructs

```gas
/*
SHIFT reg, bits
* shift left register reg by a by given num of bits
* if b is negative do a right-shift instead
*/
.macro SHIFT reg, bits
.if \bits < 0
mov \reg, \reg asr #\bits
.else
mov \reg, \reg lsl #\bits
.endif
.endm

@@ can be invoked with or without argument names
SHIFT r0, 3
SHIFT reg=r0, bits=3
SHIFT r2, -2
SHIFT reg=r2, bits=-2
```

### Chapter 2 questions

```plain
2.1a

The `.data` section is for data initialized with some values and is included in
the binary.

The `.bss` is for uninitialized data and is allocated by the loader when the
binary is loaded in memory (so is not part of binary). Data in the `.bss`
section is initialized to 0.

2.1b

`.ascii` allocates space for the given string
`.asciz` allocates space for the given string and appends a null byte

2.1c

`.word` and `.long` do the same thing. They both allocate 4 bytes of space in
the current section.

2.2

`.align` increments the location counter in the current section so that it
lands on a desired half-word or word boundary.

`.align 2` increments the location counter so that the two least significant
bits are 0 i.e. the address counter is a multiple of 4

2.3

The four main elements of assembly language are

1. assembler directives
1. comments
1. executable instructions
1. labels

2.4

.asciz "Segmentation fault"

.ascii "Segmentation fault", 0

.ascii "Segmentation "
.asciz "fault"
```

END CHAP 2

## Chapter 3

* the constraints in CPU hardware are directly visible in the assembly language
* the available instructions and permissible options in an assembly language is
  shaped by the hardware that implements it.
    * e.g. if your ALU has two inputs and only one of them has a bit-shifter
      inlined then only one of the inputs can be bitshifted in a single
      instruction
    * you could achieve the same thing with multiple instructions but to take
      proper advantage of it you need to know the hardware layout.

* ARM chips can only do computation of data that is already in a register
    * => you must first load the data you want to work with into registers and then work on it

* ARM has two data buses: A, B
    * A goes directly into ALU
    * B goes through a shifter before going to ALU
        * => the B (second) operand of instruction can be shifted an arbitrary amount before hitting the ALU

### Registers

ARM provides

* 11 "general purpose" (i.e. have no special hardware reading/writing them) registers
    * r0 -> r10
        * have no other roles
* 3 general purpose registers which have special uses by convention (but not by hardware)
    * r11 (fp)
        * frame pointer
        * holds a pointer to somewhere in the stack
        * used by compilers to track the current stack frame - helps with debugging
        * you can turn this off in gcc - see `-fomit-frame-pointer`
        * you should use frame pointers if you want your assemler to be debuggable the same way higher level lang code is
    * r12 (ip)
        * inter procedure scratch register
        * used by the C library when calling functions in dynamically linked libs
        * _its contents may change, seemingly at random when functions like
          printf are called_ so don't use it as a general purpose register
    * r13 (sp)
        * stack pointer
        * by programming convention only (no hardware relies on the stack - you could choose not to use it)
        * no special instructions involve the `sp` from hardware pov it is a plain ol' register
        * holds the address of the word of data at the top of the stack
        * it does **not** hold the address of the next empty location above the stack
* 3 special registers (i.e. their contents will be changed out from under you
  if you try to use them as general purpose registers)
    * r14 (lr)
        * link register
        * can be modified directly by your code
        * is also modified as a side-effect of other instructions e.g. `bl`
        * holds a pointer to somewhere in the instruction stream
    * r15 (pc)
        * program counter
        * holds a pointer to somewhere in the instruction stream
        * automatically incremented by 4 after each instruction is fetched from memory
            1. fetch a 4byte instruction from memory location given by PC
            2. add 4 to the PC
            3. run the instruction we fetched
                * this instruction may modify the value of PC (or not)
            4. goto step 1.
        * by the time your instruction is running the pc contains the
          **address** of the **next** instruction to be executed (because the CPU
          has already incremented it)
        * can be modified directly by your code
        * is also modified as a side-effect of other instructions e.g. `b`, `bl`
    * CPSR (current program status register)
        * includes four condition flag bits which can be used to do conditional execution of instructions
            * C (carry flag bit)
                * set to 1 if
                    1. an add operation results in a carry out of the most significant bit
                        * ???
                    2. if a subtract operation results in a borrow
                        * ???
                    3. for shift operations if the last bit shifted out was a 1
            * Z (zero flag bit)
                * set to 1 if the result of an operation was 0
                * otherwise it alway set to 0
            * N (negative flag bit)
                * set to 1 if the **signed** result of an operation is negative
                * i.e. if the result is 2s compliment and the high-order bit is set then this flag will also be set
            * V (Overflow flag bit)
                * set to 1 if
                    * for addition or subtraction a **signed** overflow occurs
        * all other bits are used for operation of bare-metal programs
        * **the meaning of the flag depends on the instruction that set the flag**

UP TO SECTION 3.3

There are four kinds of ARM instruction

1. ?
1. ?
1. ?
1. ?

### Condition modifiers

* Most ARM instructions can have a _condition modifier_ attached.
* adding `s` to the instruction tells it to set flags in the CPSR register

There are 15 condition modifiers which can be added to an instruction to read/use the flags in the CPSR register:

1. al
    * always
    * is default condition
2. eq
    * Zset
3. ne
    * Zclear
4. cs (or `hs`)
    * Cset
5. cc (or `lo`)
    * Cclear
6. mi
    * Nset
7. pl
    * Nclear
8. vs
    * Vset
9. vc
    * Vclear
10. hi
    * Cset && Zclear
11. ls
    * Cclear || Zset
12. ge
    * (Nset && Vset) || (Nclear && Vclear)
13. lt
    * (Nset && Vclear) || (Nclear && Vset)
14. gt
    * Zclear && ((Nset && Vset) || (Nclear && Vset))
15. le
    * Zset || (Nset && Vclear) || (Nclear && Vset)

**setting** and **using** a condition modifier are orthogonal operations i.e.
in a single instruction you can use a modifier **and** set one for the next
instrution

* `addeqs`
    * add instruction
    * `s` means it will mutate the CPSR register as part of its result
    * uses the `eq` modifier so it will only run the instruction if the Z flag is set
* QUESTION: does `addseq` work?

UP TO 3.3.2

### Immediate data

* data that is either
    1. encoded direclty in the instruction stream
    2. put in a "literal table" at the end of the text section
        * an instruction references a piece of data in the literal table by
        adding an offset (calculated by the assembler) to the value of `pc` for
        that instruction
        * This allows for immediate data that is too large to fit in a single instruction

