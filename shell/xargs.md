# xargs

* take lines from STDIN and a command and run that command on the input lines
* defaults to trying to concatenate as many lines of input onto a single line
  as possible (staying within shell boundaries) so that it has to run your
  command less often.

```sh
{command-that-outpus-lines-on-stdout} | xargs -t {command-to-run}
{command-that-outpus-lines-on-stdout} | xargs -tI aaa {command-to-run} aaa {other-stuff}

# will create as long a command line as possible to pass to `ls -lh`
find . -iname *foo* | xargs ls -lh

# pass each line of input separately to `ls -lh`
#   * this is probably what you want
#   * -- runs command more often
find . -iname *foo* | xargs -n1 ls -lh

# pass each line of input separately to `ls -lh`
#   * not needed for simple command like `ls -lh` but handy for complex commands
find . -iname *foo* | xargs -I xxx ls -lh xxx
```

Useful options:

* -I {placeholder} lets you specify what placeholder to use in the command e.g.
  `aaa`
    * you don't have to pick a placeholder like `{}` that the shell might
      interfere with!
* -t echo the command that will be run before running it
* -n1 call the command for every n lines from STDIN. Not needed if you use `-I`
