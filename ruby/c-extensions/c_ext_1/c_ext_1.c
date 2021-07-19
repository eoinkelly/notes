#include <ruby.h>

// this function name must be of form Init_{name of this file}
void Init_c_ext_1(void)
{
    // create a :puts symbol from a string literal
    ID id_puts = rb_intern("puts");

    // create a a ruby String (allocated on heap, wired into ruby GC)
    VALUE hello_world_str = rb_str_new_cstr("Hello world!");

    // Call Kernel.puts("Hello world!")
    // third arg is num args passed to puts
    rb_funcall(rb_mKernel, id_puts, 1, hello_world_str);
}
