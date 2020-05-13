# R

## Sources

* Book: R in Action
* Book: R for Data Science https://r4ds.had.co.nz/index.html
    * Documents the Tidyverse
* https://www.lynda.com/Martin-Hadley/4174500-1.html
    * heard this guy on a podcast, he seems like good teacher

## Overview

* R is a bit like old-school JS
    * Bit crusty as a language but lots of libs/frameworks have evolved to work around it's crustier bits
* A huge amount of the momentum in the R community seems to be around Rstudio and Tidyverse
    * Without it R would be quite crusty and **much** harder to interface with other stuff
* Around since the late 1990s
* Aside: Intel _Math Kernel Library_
    * [Intel MKL](https://software.intel.com/en-us/mkl)
    * MKL is a cross-platform C lib of math routines optimized for Intel processors
    * has C and Fortran interfaces
* Distros of R
  * CRAN https://cran.r-project.org/
        * "CRAN R" does not bind to [Intel MKL](https://software.intel.com/en-us/mkl) libs for matrix operations
        * If you want to do matrix operations efficiently in R then the Microsoft version of R is recommended
  * MRAN https://mran.microsoft.com/
        * Officially supports Windows and Linux.
        * macOS homebrew has `microsoft-r-open` package but it crashes on launch for me in Catalina
        * Microsoft R Open is a downstream distribution of R with added components. As a result it is 100% compatible with the latest version of R, and will continue to be so.
* R consortium
    * manages CRAN the package manager
* purpose
    * statistical analysis
    * data manipulation
* culture
    * stable, they don't "move fast and break things"
    * Rstudio is the standard IDE (seems to be the "photoshop" of the R world)
    * lots of users use R in Rstudio without thinking of themselves as "programmers"
* is great at
    * rectanglar data with different types from database/spreadsheet/files
* not ideal for
    * running numerical simulations - use Python for this instead
    * There are lots of datasets that are not naturally rectangular including images, sounds, trees, and text.
        * R might not be the best choice for these but I'm not sure about that
* has muiltiple implemenations of the _data frame_ idea
    1. base R data frame
        * dated, has some irritating features
        * indexes by column then row
        * fine for datasets that can fit in memory or be loaded incrementally from a datastore like a DB (up to a few GB depending on your box)
    1. tibble data frame (from tidyverse)
        * indexes by column then row
        * fine for datasets that can fit in memory or be loaded incrementally from a datastore like a DB (up to a few GB depending on your box)
    1. datatable
        * https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
        * designed for huge datasets
        * indexes by row then column (sometimes used on small datasets by people who prefer this)
        * needed for datasets that can't fit in memory
* R has no native way to isolate packages to a single project
    * Packrat is an attempt to solve this
        * http://rstudio.github.io/packrat/
        * it vendors your packages into your project dir
    * renv is basically Packrat 2.0 and is probably what you should use
    * Note that rsconnect (the thing that publishes to shinyapps.io) does not support renv.lock
* All character strings in R stored in a single global hash (like symbols are in Ruby)
    * => more efficient for strings which appear a lot in data sets.
    * => No GC of string data - if you load lots of transient strings you'll probably eat memory ???
* tidyverse
    * started in 2015
    * a curated collection of modern R packages for data science
    * packages work together well
    * encourages the use of tibbles by default
    * tidyeval ???
    * packages included
        * purrr (functional programming toolkit) https://purrr.tidyverse.org/
            * defines a `%>%` pipeline operator
* there is also "validated" version of R called _ValidR_ for use environments which require that e.g. drug trials
    * https://www.mango-solutions.com/products/validr/
* an open source port of `S` lang (sometimes called "Gnu S")
* "has both functional and OO features"
    * I'm not sure author understands these words the same way I do
* has interactive sessions/repl
    * interactive mode saves command history
        * comments are saved in the command history (helps when trying to
          understand your history)
        * saves to `.Rdata` file in cwd
    * interactive mode prompts you to saves data sets between sessions too!
* like C `{}` can be omitted for single line blocks
    * unlike C you can do this for function definitions too
* assignment
    * operator is `<-` or `=` but `<-` is preferred because sometimes `=` does not work
        * TODO find out more about this
    * you can do assignment in the reverse direction but it is not recommended stylistically e.g. `c(1,3) -> foo` works
* line comments begin with `#`
* R has no scalar values
    * numbers are considered one element vectors
* variables do not have fixed types
* any value which can be assigned is called an "object"
    * all values have a "storage mode" which is basically the type. You can introspect it with `mode()`
    * all values have a ???
* `.` is not used for attribute reference or method calls - it is just a legal character in a variable name that is used to indicate some sort of heirarchy in the name
    * everything is pass by value which helps avoid bugs but is inefficient


## R types


* `NULL`
    * is returned by functions that don't explicitly return anything e.g. `print()`, `plot()`
* boolean
    * `TRUE` and `FALSE` (or `T` and `F` as shorthand)
* numbers
    * ?? R doesn't have a 64 bit integer type?
* vector
    * all elements must be same type (or "storage mode")
    * INTUITION WARNING: vector indices begin at 1 not 0
    * created with `c()` global function
    * can optionally have a name (string) associated with each element
* character strings
    * INTUITION WARNING: are NOT considered a vector of characters!
    * each string is a **single element** vector of mode character
        ```r
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
    * all objects must be same type
        ```r
        > m <- rbind(c(1,4),c(5,7))
        > m
            [,1] [,2]
        [1,]    1    4
        [2,]    5    7
        ```
    * you can use the `,` within the square bracket notation to get a whole column or whole row
* functions
    * can have default args ` foo <- function(a=1, b=TRUE, c=F)`
    * function application is C alike
    * have implicit returns but you can use an explicit `return()` statement
        * doing so is recommended by style guides
    * can have generic functions (same function name does diff things for diff types) e.g. `plot()` will try to do something appropriate with its given data
    * IMPORTANT: R **copies** all arguments to functions
        * R is totally pass by value even for complex data structures
        * => functions can mutate their args and changes are ignored
        * this has performance implications!
* scoping
    * variables created outside functions are global
    * variables declared within a function are local to that function
    * a global var can be written from within a function using the "super assignment" operator `<<-`
* Style and linting
    * https://style.tidyverse.org/ seems to be the common one
        * google has a slight variation on it
    * autoformatter: http://styler.r-lib.org/ (formats to the tidyverse style guide)
    * linter: https://github.com/jimhester/lintr
* R startup
    * R sources quite a few files at startup which allow customisation of the environment
    * System Rprofile: `/usr/local/Cellar/r/3.6.2/lib/R/library/base/R/Rprofile`
    * Run `?Rprofile` in repl to get help about R startup
    * `~/.Rprofile` will run at startup and is the best place to put code for all your R projects
* Reading nviornment variables
    ```r
    Sys.getenv("R_PROFILE") # => ""
    Sys.getenv("R_HOME") # => "/usr/local/Cellar/r/3.6.2/lib/R"
    ```
* packages
    * install with `install.packages("name1", "name2")`
    * update with `update.packages(ask = FALSE)`
    * R packages seem to involve a lot of C compilation
        * my guess is the community drops back to C/C++ fairly readily
    * almost all R packages I installed seem to require C compilation
    * I installed:
        ```sh
        # data science
        tidyverse

        # better graphs
        ggplot2
        gridExtra

        # a package to accompany a book: # Companion to Applied Regression
        # I don't really know why I installed this - the book told me ¯\_(ツ)_/¯
        car

        # web framework
        shiny
        ```
    * packages commands:
        ```r
        install.packages("nameofpackage")
        installed.packages()
        ```
* S3 and S4
    * some kind of objects ???
    * there are functions to test whether an object is S3 or S4 and to convert between them

## Accessors

R has three accessor operators

1. `[]`
   * the "subset" operator
   * often returns the same type but not always e.g. if you pull a column out of data frame you might get a vector (you can control this by adding `drop=FALSE` as an option)
   * mostly returns the same type as the object you run it on e.g. vectors return vectors, lists return lists.
       * An exception is that data frames sometimes return a different type e.g. it could return a vector - you can use `drop=FALSE` option to ensure it always returns a data frame
        ```r
        class(iris[, "Petal.Length", drop=FALSE])
        [1] "data.frame"
        > class(iris[, "Petal.Length"])
        [1] "numeric"
        ```
2. `[[]]`
    * extract **one** item
        * examples:
          * using it on a data frame gives you a vector
          * using it on a list gives you one element
          * using it on a vector gives you one element
    * ++ can use named or integer indices
    * ++ can interpolate variables to generate the index to find
    * the result can be a different type than the thing you run it on
    * you get back exactly **one** thing
3. `$`
    * a special case of `[[]]` where you use the name of a column or row
    * wrap names with special chars in backticks
    * -- cannot interpolate variables for the name of row or column
    * -- can only use named rows/cols - doesn't work with integer indices


Question: I'm not sure waht the diff is between them tbh

## Installation

```bash
# macOS

brew install r # CRAN R
brew cask install microsoft-r-open
```

## Getting help

```R
# get help about function
?funcName

# see examples
??funcName
```

## Introspection

```R
x <- c(1,44,55)

# can do basic introspection in the reply by just typing the name of the "object"
x
print(x) # same as above in the repl
# [1]  1 44 55

# str() prints a short summary of the structure of the object
str(x) # => "NULL"
# num [1:3] 1 44 55

# returns a string of a table of summary data
summary(x)

# generate the R code required to create this value
dput(x)
```

## Syntax examples

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

print("hi")
# "hi"

# functions are created in a fairly normal way
foo <- function() {
  print("hi")
}

# return storage mode as a string
mode(foo)
# => "function"

# introspect the type
typeof(foo)
# => "function"

foo()
# "hi"

# variable names can be re-used
foo <- function(a) {
    print(a)
    print(mode(a))
}

foo(23)
# 23
# "numeric"

getwd()
[1] "/Users/eoinkelly/Dropbox/Eoin/Notes/on-github/R"

# generate a random uniform distribution
runif(20)
```

## Ingesting data

Remember you probably want `stringsAsFactors=FALSE` in these functions

```r
read.table()
read.csv()
data.frame()
```
