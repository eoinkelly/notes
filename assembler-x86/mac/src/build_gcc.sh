#!/bin/bash

# Usage:
#     ./build_gcc.sh foo
#
# will build foo/foo.s into foo/foo

# be verbose (-v) and echo variable expansions (-x)
# set -vx

name=$1

yasm -f macho64 -l "${name}/${name}.lst" -o "tmp/${name}.o" "${name}/${name}.s"

# if the assembly defines `main` label instead of `_start` you must link with
# gcc which will provide its own `_start` point.
gcc -o "bin/${name}" "tmp/${name}.o"
