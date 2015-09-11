#!/bin/bash

as hello.s -o hello.o
ld hello.o -e _main -o asm_hello

