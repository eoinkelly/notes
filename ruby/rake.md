# Rake

http://devblog.avdi.org/2014/04/30/learn-advanced-rake-in-7-episodes/

- rake will trakc file modificaiton times to skip certain files on rebuilds

- rake tasks can depend on other tasks or files
- when you depend on a file, the task name is the file name

- tasks
    - plain tasks
    - file tasks
        - same as plain tasks except if rake can find a file on disk matching
          the name of the task it will use its modificaiton time to see if it
          can skip running the task

- Rake modifies String to support some of the same methods that FileList
  supports
    - allows you to use file lists and individual files interchangeably
    - #ext
    - #pathmap

- pathmap is pretty cool
    - a specialised function for converting one filepath into another

- rake provides all the helpers from ruby FileUtils lib

- prerequesites (dependencies) can be lambdas if you need to evaluate something

```
rule '.foo' => -> { |?| do_stuff } do |task|
  # command to build things
end
```

you can pass rake filenames as command line args because rake considers
filenames to be task names

- dependencies
    - rake tasks can depend on
        1. other tasks
        1. files on disk
- rules
    - tell rake how to create a particular filetype

#file

- tells rake how to build a given output filename (path?) from a given input
  filename
- -- only instructs rake about how to build a single output file at a time

```ruby
file output_filename => input_filename do
  sh "converter #{input_filename} -o #{output_filename}"
end
```

#rule

- more general than `#file` - tells rake how to build all files of a given type

```ruby
rule '.html' => '.md' do |t| # t is instance of Rake::Task
  sh "pandoc -o #{t.name} #{t.source}"
end
```

# File::List

```ruby
list = Rake::FileList['**/*.md', '**/*.markdown']
list.exclude('*.swp')
list.exclude(/some regexp/)

# exclude files for which the block returns true
list.exclude do |filename|
  `git ls-file #{filename}`.empty?
end
```
