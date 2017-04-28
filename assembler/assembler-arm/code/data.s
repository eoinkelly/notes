@@ vim: ft=armasm

@@ This is assembly
@@ and this is more

                .data
byties:         .byte   'A', 'B', 0, 230
num_1:          .hword  333
num_2:          .word   5555
greeting:       .asciz  "hello there"
long_greet:     .ascii  "hello there\n"
                .ascii  "my name is eoin\n"
                .asciz  "and this is some code\n"       @ some comment
pi:             .double 3.1459

