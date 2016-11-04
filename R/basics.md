# R lang

* purpose
    * statistical analysis
    * data manipulation
* an open source port of `S` lang (sometimes called "Gnu S")
* "has both functional and OO features"
    * I'm not sure author understands these words the same way I do
* interactive sessions
    * interactive mode saves command history
        * comments are saved in the command history (helps when trying to
          understand your history)
    * interacive mode saves data sets between sessions too!
* like C `{}` can be omitted for single line blocks
    * unlike C you can do this for function definitions too
* types
    * boolean
        * `TRUE` and `FALSE` (or `T` and `F` as shorthand)
    * vector
        * all elements must be same type (or "storage mode")
    * character strings
        * INTUITION WARNING: are NOT considered a vector of characters!
        * each string is a **single element** vector of mode character
        ```
        > a <- "hello"
        > a
        [1] "hello"
        > mode(a)
        [1] "character"
        > a[1]
        [1] "hello"
        > a[2]
        [1] NA
        ```
    * matrices
        * a vector that has also
            * a number of rows
            * a number of cols
        ```
        > m <- rbind(c(1,4),c(5,7))
        > m
            [,1] [,2]
        [1,]    1    4
        [2,]    5    7
        ```
* scoping
    * variables created outside functions are global
    * variables declared within a function are local to that function
    * a global var can be written from within a function using the "super assignment" operator `<<-`
* functions
    * can have default args ` foo <- function(a=1, b=TRUE, c=F)
    * function application is C alike
    * have implicit returns but you can use an explicit `return()` statement
    * can have generic functions (same function name does diff things for diff types) e.g. `plot()` will try to do something appropriate with its given data
    * IMPORTANT: R **copies** all arguments to functions
        * R is totally pass by value even for complex data structures
        * => functions can mutate their args and changes are ignored
        * this has performance implications
* line comments begin with `#`
* R has no scalar values
    * numbers are considered one element vectors
* variables do not have fixed types

Stats basics

* mode = most frequently occuring number
* mean = average, `sum(nums) / count(nums)`
* standard deviation = ???

```R

# * c() concatenates vectors into a vector
# * assignment happens by <- or = (but <- is prefered because there are
#   situations where = will not work
x <- c(1,2,3)

x[1] # get first element of vector
x[2:3] # get range within vector


# functions
q() # quit - you will be given option to save workspace image (which includes command history)
source("path/to/file.R")
mean()
sd() # standard deviation
mode() # returns the "storage mode" (or type) of an object, does not calculate the statistical mode
print() # print to stdout
paste() # concatenate strings
strsplit() # split strings

rbind(v1, v2, ...) # build a matrix using provided vectors as rows
cbind(v1, v2, ...) # build a matrix using provided vectors as colums
```

