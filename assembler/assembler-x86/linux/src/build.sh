#!/bin/bash

# Usage:
#     ./build.sh foo
#
# will build foo/foo.s into foo/foo

# be verbose (-v) and echo variable expansions (-x)
# set -vx

name=$1

yasm -f elf64 -g dwarf2 -l "${name}/${name}.lst"  -o "tmp/${name}.o" "${name}/${name}.s"
ld -o "bin/${name}" "tmp/${name}.o"
