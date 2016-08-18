
GCC file extensions

* `.s` = assembly file
* `.S` = assembly file that must be preprocessed with the C pre-processor (CPP)
    * handy if you are mixing C and assembler in a project and you want to make use of your C macros

The C preprocessor does things like #define #ifdef etc.

But arm assembly provides `.eq` and `.ifdef` so we probably don't need the C preprocessor
    is this true ???

