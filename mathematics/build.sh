#!/bin/bash

pandoc basics.md -f markdown+tex_math_dollars -t latex -o basics.latex -s && pdflatex basics.latex
