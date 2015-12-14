# File IO in ruby

* `IO` is the parent of all I/O classes in ruby
    ```
    File < IO < Object < BasicObject
    TCPSocket < IPSocket < BasicSocket < IO < Object < BasicObject
    UDPSocket < IPSocket < BasicSocket < IO < Object < BasicObject
    ```
* An IO has
    * modes (read only, write only, append etc.)
        * `r` read only
            * file position starts at beginning of file
        * `w+` read-write
            * file position starts at beginning of file
        * `w` write only
            * truncates existing file to 0 length and creates new file for writing
            * file position starts at beginning of file
        * `w+` read-write
            * truncates existing file to 0 length and creates new file for writing
            * file position starts at beginning of file
        * `a` append write only
            * file position starts at end of current data
            * creates new file if it does not exist
        * `a+` append read-write
            * file position starts at end of current data
            * creates new file if it does not exist
    * encodings (conversions)
        * TODO
* All the `File` things to do with opening, reading, writing, closing come from `IO`
* a ruby stream can be duplex so may use more than one OS native stream

Aside: ARGF

* `ARGF` is an IO like stream representing all files mentioned on the command
  line or STDIN if no files mentioned.
    * ARGF#path
    * ARGF#filename


* File::ALT_SEPARATOR is the platform specific file separator e.g. `\` on windows, `/` on unix


IO instances contain #eof? which will return true if the file handle is at the end of the file contents

## Internal vs external encoding

???

## Kernel#open

* creates an IO object
* can be
    * a file "foo.txt"
    * a subprocess "|ls"
    * another ruby instance as a subprocess "|-"

## Opening

There are 3 phases to working with a stream

1. open the stream
1. read and/or write to it
1. close the stream

## Opening

* `IO.new`
    * used by ::open, Kernel#open, File.open
* `IO.open`
    * if called without a block it is an alias for `IO.new`
    * called with a block it runs `IO.new` on its args and passes the resulting
      IO to the block
    * returns the return value of the block

## Reading

* `IO#getbyte`
    * read a byte from the IO
    * return `nil` if EOF
* `IO#getc`
    * read a character from the IO
    * return `nil` if EOF
* `IO#gets`
* `IO#readline`
* `IO#readlines`
* `IO#readchar`
* `IO#readbyte`

## Opening + reading + closing combined

* `IO#read`
    * open, optional seek, read given num of bytes, close, return
* `IO.binread`
    * same as IO.read except it forces encoding to ASCII-8BIT
* `IO.readlines`
    * open, optional seek, read all lines into array, close, return array

## Writing

* `IO.write`
    * open, optional seek, write given string, close, return num bytes written
* `IO.binwrite`
    * same as IO.write except it forces encoding to ASCII-8BIT
* `IO.putc`
* `IO.puts`

## Opening + writing + closing combined

* `IO.foreach`
    * open, execute block for each line, close

## moving around in a file

* `IO#seek`
* `IO#rewind`
* `IO#pos`

## Closing

* `IO#close`
    * Close the stream and flush any pending writes to the OS
    * the stream will be unavailable after this
        * The IO will raise an `IOError` if something tries to access the stream after it has been closed.
    * File operations don't happen immediately - writes get buffered by the OS
      and by the disk subsystem.
    * Calling close on a file forces a flush of these buffers so while you
      might get away with not doing it it is good practice to do so.


## Examples

```ruby
File.foreach('testfile') {|x| print "GOT", x }
IO.foreach("testfile") {|x| print "GOT ", x }

File.read(file_name)
File.readlines 'file.txt'

File.open("my/file/path", "r") do |f|
  f.each_line do |line|
      puts line
        end
        end
# File is closed automatically at end of block

f = File.open("my/file/path", "r")
f.each_line do |line|
  puts line
end
f.close


file = File.open("sample.txt", "r")
contents = file.read # grabs all file contents
file.close

contents = File.open("sample.txt", "r"){ |file| file.read }


# readlines reads "lines" of file delimited by ??? into an array
File.open("sample.txt").readlines.each do |line|
  puts line
end

# if sample.txt is very big this method will not read the whole thing into memory
file = File.open("sample.txt", 'r')
while !file.eof?
   line = file.readline
   puts line
end
```

## writing to file

```ruby
somefile = File.open("sample.txt", "w")
somefile.puts "Hello file!"
somefile.close

File.open("sample.txt", "w"){ |somefile| somefile.puts "Hello file!"}

# kernel open

my_local_file = open("my-downloaded-page.html", "w")

my_local_file.write(remote_data)
my_local_file.close
```
