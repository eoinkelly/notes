# VSCode Ruby

## My current setup

```
Solargraph
    Runs rubocop
    Intellisense
    Code folding
Debugging (nothing)
    rebornix.ruby could do it but I prefer CLI debugging via binding.pry
dead_end
rebornix.ruby
    has a bunch of syntax highlighting/folding etc.
```

Set up for work on a new project:

1. Install the gems I need:

    ```ruby
    # Nice editing environment helpers
    gem "pry-byebug"

    gem "rubocop"
    gem "rubocop-performance"
    gem "rubocop-rails"
    gem "rubocop-rspec"

    gem "dead_end"
    gem "solargraph"
    ```

2. Run
    ```
    bundle exec solargraph download-core
    yard gems
    bundle exec rubocop --server
    ```

Issues:

- Sometimes solargraph doesn't notice that files were deleted and you need to
  restart the server

## Background

- LSP
    - Language server protocol
    - https://microsoft.github.io/language-server-protocol/
    - Created by Microsoft for VSCode but used elsewhere now
    - Provides intellisense features
        - context aware autocompletion
        - access to inline documentation
        - an output for various linting and diagnostics tools
        - type checking

## Features you can have

- Snippets
- Automatic Ruby environment detection
- Run external tools and report the results inline in your code via the LSP
- Autoformat code
- Semantic code folding support
- Semantic highlighting support
- Basic Intellisense support (context aware autocompletion)
- Integrate a Ruby debugger with the VSCode Debugger

## Overview

There is a LOT of overlap in functionality between addons e.g. many want to run
rubocop for you, **many** have snippets

## Addons

- rebornix.ruby (Ruby)
    - https://marketplace.visualstudio.com/items?itemName=rebornix.Ruby
    - does not enable formatting or linting by default
    - can run rubocop for you
    - is currently unmaintained
    - has an embedded LSP implementation which isn't enabled by default yet
        - https://github.com/rubyide/vscode-ruby/blob/main/docs/language-server.md
    - dependencies installed by it:
        - wingrunr21.vscode-ruby (VSCode Ruby)
            - > improved syntax highlighting, language configuration, and
              > snippets to Ruby and ERB files
    - > In this extension, we implement ruby debug ide protocol to allow VS Code
      > to communicate with ruby debug, it requires ruby-debug-ide to be
      > installed on your machine. This is also how RubyMine/NetBeans does by
      > default.
    - snippets
        - https://github.com/rubyide/vscode-ruby/tree/main/packages/vscode-ruby/snippets
        - (technically part of wingrunr21.vscode-ruby but is installed as part
          of rebornix.ruby)
- zombocom.dead-end-vscode
    - needs the dead_end gem installed
- shopify.ruby-lsp (Ruby LSP)
    - by Shopify
    - fairly new, since March 2022
    - communicates and requires the ruby-lsp gem
    - docs are very minimal
      https://shopify.github.io/ruby-lsp/RubyLsp/Requests.html
    - only properly supports `shadowenv` as a version manager, support for other
      version managers is planned (as of Aug 2022)
    - Roadmap https://github.com/Shopify/ruby-lsp/issues/206
        - hints that this gem is designed to be used with the sorbet-lsp, will
          this gem be good if you don't use Sorbet in the long term?
    - Doesn't integrate with other tools like rubocop (yet)
        - they seem to be working with dead_end
    - CONCLUSION: still too early to use but worth watching
- castwide.solargraph (Ruby Solargraph)
    - relies heavily on Yard docs
        - you need to build them for your installed gems
        - there are some workarounds to get Yard docs for Rails
          https://solargraph.org/guides/yard
    - has a Patreon
    - you can download core ruby docs with `solargraph download-core` but the
      most recent is 2.7.4
    - > Although Solargraph can work with projects that have no YARD
      > documentation at all, using tags like @return will not only give you
      > better intellisense results, it will improve the overall performance of
      > the language server by requiring less code analysis during editing
    - comms with the solargraph gem (which provides a language server)
    - solargraph will run rubocop for you and comm the results to VSCode via the
      LSP
    - solargraph gem
        - is an implementation of the LSP
        - supports many editors
    - can run rubocop for you but doesn't bundle it
    - creator says it is designed to work alongside rebornix.ruby
    - CONCLUSION: probably the right choice for now
- hridoy.rails-snippets (Ruby on Rails)
    - a big bucket of snippets
    - not updated since 2018
    - https://github.com/hridoy/rails-vscode
        - looks like the github repo just exists because you need one for the
          VSCode marketplace links - it doesn't contain the snippets :-(
        - nobody can fork it or contribute fixes
    - The snippets are not actually in the github repo but you can see them in
      ~/.vscode/extensions/hridoy.rails-snippets-1.0.8
    - CONCLUSION: still useful but can't be updated easily :-(
- ruby-syntax-tree.vscode-syntax-tree
    - https://marketplace.visualstudio.com/items?itemName=ruby-syntax-tree.vscode-syntax-tree
    - looks raw, not much in the way of docs
    - uses the LSP provided by stree
    - intended to be a full "rewrite my file"/`go fmt` style formatter
        - minimal options (only print width AFAICT)
    - CONCLUSION: seems cool but early and not sure how well it plays along with
      other plugins
