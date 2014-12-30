# Kata 1

Following the iOS naming conventions as much as possible, create a class:

```
BlogPost
    properties
        title
        date
        body
        published (defaults to false)
            use custom getter and setter names for `published`
                * isPublished
                * markAsPublished
        creationDate (read only, defaults to today)
        ---
        authors (an array of strings)
    public methods
    + init
    + initWithCreationDate
    - titleAsUpperCase // self explanatory
    private methods
    - contentLength // no. of chars in title+body
```

Notes:

* Do all the below using test first (have a failing test before implementing the functionality)
* Use message sending style for everything

Instructions:

1. create all instance variables and getters/setters manually
    * make a copy of authors for memory safety
3. re-implement them with `@property` syntax
4. create all instance methods and class methods
5. use `#pragma mark - section` to make nice sections
6. Refactor the code to use dot notation


TO SOLVE

* how to test a private method in objC
* how to test that an object takes a copy of a provided NSString or NSArray
