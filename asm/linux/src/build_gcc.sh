#!/bin/bash

# be verbose (-v) and echo variable expansions (-x)
# set -vx

name=$1

yasm -f elf64 -g dwarf2 -l ${name}.lst ${name}.s
# ld -o ./bin/${name} ${name}.o

# if the assmebly defines `main` label instead of `_start` you must link with
# gcc which will provide its own `_start` point.
gcc -o ./bin/${name} ${name}.o
