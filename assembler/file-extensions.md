# Assembly file extensions

`gcc` file extensions:

* `.s` = assembly file
* `.S` = assembly file that must be preprocessed with the C pre-processor (CPP)
    * The assembler also has a macro system e.g. ARM assembler provides `.eq` and `.ifdef` macros
    * This file type is handy if you are mixing C and assembler in a project and you want to make use of your C macros
