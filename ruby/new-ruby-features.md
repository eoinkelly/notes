# Ruby features I should try to start using

Taken from reading https://rubyreferences.github.io/rubychanges/evolution.html

## Consider

- one line method definitions

## Use

- numbered block params (since 2.7)
    ```ruby
    [1,1,2].map { _1 * 10 }
    ```
- const_source_location
    ```ruby
    p B.const_source_location('C4') # => ["test.rb", 12]
    ```
- Integer#[] to get bits out of a number
- String#b prefix
    ```ruby
    [53] pry(main)> aa = "hi"
    => "hi"
    [54] pry(main)> aa.encoding
    => #<Encoding:UTF-8>
    [55] pry(main)> aa.b.encoding
    => #<Encoding:ASCII-8BIT>
    ```
