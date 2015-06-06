# Empty things

* there is no null or void type
* uses `()` as the convertion to denote something empty
* the type of `()` is `()`
    ```
    :t ()
    () :: ()
    ```
* `undefined` is used to represent some sort of error state - it is not something
  to expect as return value etc.
    ```
    :t undefined
    undefined ::
    ```
    * it is a polymorphic constant i.e. a value that can take on any type (no
      restrictions)
