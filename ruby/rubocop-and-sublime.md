# Make rbenv, rubocop and Sublime Text 3 work on Mac OS X

If you are using `rbenv` to manage ruby versions and want to use `rubocop` as a
live linter for ruby files in Sublime do:

1. Install `SublimeLinter` sublime text package
1. Install `SublimeLinter-rubocop` sublime text package
1. Ensure that you have set a "global" ruby in `rbenv` i.e. the output of
   `rbenv global` should be whichever ruby version you want to run rubocop in.
   If there is no global ruby set yet you must set it now.
1. Install rubocop gem in your chosen rbenv managed ruby via
   `gem install rubocop`
1. Ensure that your macOS PATH variable is setup so that Sublime Text will find
   your rbenv ruby _before_ it finds the system ruby. To do this
    1. Find where your rbenv ruby is via `which ruby`
        ```
        $ which ruby
        /somepath/somewhere/.rbenv/shims/ruby
        ```
    1. Add the _directory_ that contains ruby to the **top** of `/etc/paths`
       e.g. in this case you could do `sudo $EDITOR /etc/paths` and put
       `/somepath/somewhere/.rbenv/shims` at the top of the file.

        Aside: `/etc/paths` sets paths on OS X for all apps - even those that do
        not use a shell.

If it still isn't working you can diagnose what is happening in Sublime:

1. In sublime go to Tools > Sublime Linter > enable debug mode
1. Open the sublime console via CTRL-` shortcut
1. Check that ST3 is using correct ruby version and that the PATH (as echoed in
   the ST3 console has the rbenv shims dir at the top (so your rbenv ruby will
   be found before the system ruby)
