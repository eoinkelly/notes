# Haskell pragmas

```
{-# word ... #-}
{-# word ...
  #-}

```

* the closing `#-}` must start in a column to the right of the opening `{-#` - this is called the "layout rule"
* `word` must be recognised by the compiler

## File header pragmas

* must precede the `module` keyword in the file
* valid words are
  * LANGUAGE
      * specify language extensions (ideally all compilers will use this)
  * INCLUDE
      * Include header files for any C files you are building
      * No longer required for GHC but there for compatibility
  * OPTIONS_GHC
      * specify options for GHC compiler


## Other pragmas (that are in Yesod)

* UNPACK
    * unpack the contents of a constructor field into the consturctor itself
    * it removes a level of indirection
    * `data Foo = Foo {-# UNPACK #-} Float` ??? is this correct?
    * it is a compiler optimisation
* IF/ELSE/ENDIF
    * I think these are C compiler macros
