`gcc` file extensions

* `.s` = assembly file
* `.S` = assembly file that must be preprocessed with the C pre-processor (CPP)
    * handy if you are mixing C and assembler in a project and you want to make use of your C macros

The C preprocessor does things like #define #ifdef etc.

ARM assembler provides `.eq` and `.ifdef` macros
