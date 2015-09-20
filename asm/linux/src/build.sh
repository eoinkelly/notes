#!/bin/bash

name=$1

yasm -f elf64 -g dwarf2 -l ${name}.lst ${name}.s
# link with gcc which provides its own _start
ld -o ./bin/${name} ${name}.o
