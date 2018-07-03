## Basics

* The instruction set for a GPU is not standardized.
* NVidia (and AMD and other GPU vendors) can and do change their instruction set from one GPU model to the next.

## Cuda

* Works only on NVIDIA GPUs
* Around since 2006
* for general purpose programming on Nvidia GPUs
* adds some basic keywords to languages like C, C++, Fortran, Python, Matlab
* CUDA apps run on both CPU and GPU
	* sequential stuff runs on CPU
	* parallel part of workload runs on GPU
* CUDA C/C++ provides an abstraction
* The compiler generates PTX code
* CUDA C extends C
	* you define C functions, called kernels, that, when called, are executed N
times in parallel by N different CUDA threads, as opposed to only once like
regular C functions
* PTX (Parallel Thread Execution ISA) code
	* not hardware specific (is a kind of IR I guess)
	* Compiled into instructions for the GPU by the GPU driver at **run-time**

# OpenACC

* OpenACC is an open industry standard for compiler directives or hints which
  can be inserted in code written in C or Fortran enabling the compiler to
  generate code which would run in parallel on multi-CPU and GPU accelerated
  system.

## OpenCL

* Supported by AMD CPUs
* The programming language that is used to write compute kernels is called OpenCL C and is based on C99 but adapted to fit the device model in OpenCL.
* You program in a C-like language and then OpenCL can target that code at CPUs and GPUs, and is not as tied to a specific brand of GPU hardware.
*  It defines a C-like language for writing programs.
*  Programs in the OpenCL language are intended to be compiled at run-time, so
   that OpenCL-using applications are portable between implementations for
   various host devices
