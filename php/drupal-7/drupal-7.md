# Pro Drupal 7 Development 3rd Edition

- Drupal is a sort of _application platform_
- Can run on MySQL, PG, SQLite
- Can use any webserver
    - Apache is most popular
- Drupal **needs** web server URL rewriter to do clean URLs e.g. `mod_rewrite`
  in Apache
- Drupal 7 rewrote its DB layer - the DB layer is based on _PHP data object_
  (PDO)
    - this is what allows Drupal to support multiple databases
- Drupal has coding standards for PHP, JS, CSS Twig etc.
    - https://www.drupal.org/node/318
    - https://www.drupal.org/node/318#helpermod lists some tools for doing code
      review
        - tools are shipped as Drupal modules
        - they do have a "standard" config for
          [PHP Codesniffer](https://github.com/squizlabs/PHP_CodeSniffer)
- Drupal is architected as
    1. A library of common functions
    2. A set of "core" modules
    3. You can add modules from 3rd parties or make your own
- Modules
    - Are named `something.module`
- Drupal modules interface with drupal via callbacks/hooks
    - Drupal finds your module's callback handlers via a simple naming
      convention
        - If the `hook_user_login` hook fires then Drupal will look for a
          funciton in your module called `{module_name}_user_login`
- Themes
    - Templating options:
        - _PHP Template_ (the primary Drupal templating engine)
            - most popular for making themes
        - _Easy Template System (ETS)_
    - Themes can override parts of the page by implementing a function with the
      correct name
- Nodes
    - The base object type in Drupal
        - A bit like `Object` in Ruby, everything in Drupal _is a_ node.
        - Nodes have some base behaviours
            - be promoted to front page
            - be searched
            - be published or unpublished
    - The admin interface can mix & match things in various places and batch
      edit nodes because it just depends on their "nodeness"
- Fields
    - Nodes have fields
    - Fields is the base content type in Drupal
    - node content is defined as collections of fields
    - Modules can create new field types or combine existing fields to make new
      complex types
- Blocks
    - are things which can be put into locations in your template
    - templates have placeholders for blocks and then a module or admin can fill
      in the blocks
- Installation profile
    - A set of instructions to run **after** the base Drupal install has run to
      install the site i.e.
        - which modules to enable/disable
        - which contributed modules to download (I'm not 100% it will download
          new modules for you btw)
    - You can download new installation profiles and put them in `profiles/`
        - They give you a "distribution" of Drupal tuned for a specific use-case
    - Examples (which come as standard with Drupal 7)
        - `minimal`: a minimal Drupal installation
        - `testing`: installs just what is required for testing
        - `standard`: a standard Drupal installation
- File layout

    ```bash
    includes/ # libs of common functions
    modules/ # contains core modules (do not edit)
    misc/ # JS, icons, images from the stock Drupal installation
    profiles/ # installation profiles
    scripts/ # various helper scripts: cron stuff, running test suites. Not used in request cycle
    themes/ # contains the template engine and default themes (do not modify this dir)

    sites/ # contains all your modifications to Drupal

    sites/all/modules # you install modules here

    sites/default/default.settings.php
    # * your default configuration file, used by drupal installation process to create sites/www.mysite.com/settings.php

    sites/www.mysite.com/settings.php # my site settings

    sites/default/files
    # * stores uploaded files
    # * web server needs rw permissions on this
    # * drupal installer will try to create it if it can

    sites/default/private
    # * used to store "private" files that aren't served to users unless they are authenticated in some unspecified way
    # * unclear if this is created by default or you have to create it yourself

    sites/all/themes
    # * custom themes go here

    cron.php # used to execute periodic tasks (e.g. pruning DB tables)
    index.php # main entry point for serving requests
    install.php # entry point for Drupal installer
    update.php # updates the database schema after a Drupal version upgrade
    xmlrpc.php # receives XML-RPC requests, can be safely deleted if you don't need that feature
    robots.txt # default

    authorize.php
    # * an admin script for running "authorized file oeprations" e.g. download and install a new theme or module
    # * you don't run this directly, instead you invoke some part of the Admin UI which redirects you to this script to do certain steps
    ```

- General observations
    - All the PHP code in a Drupal app is checked into your repo - all the
      modules are "vendored" in there too

### Patches

- The `patches/` dir has a number of patch files (ala git)
- They seem to patch certain composer packages
- looks like they are manually applied via the `patch` command line tool
- Reasons for using them
    1. Fix errors running old Drupal on newer PHP versions
        - these changes are checked into git
    1. Fix bugs in composer packages
        - these changes are **not** checked into git
        - Composer can patch packages via the
          https://github.com/cweagans/composer-patches plugin
            - just refernce your patch in `composer.json`

Serving a request

Consider a request for http://example.com/foo/bar

1. mod_rewrite rewrites http://example.com/foo/bar to
   http://example.com/index.php?q=foo/bar
    - the URL is assigned to the `q` query parameter
1. `index.php` executes
1. Drupal bootstraps itself on **every** request - there are ? bootstrap phases
   (defined in `bootstrap.inc`)
    1. Configuration: set global variables for use in bootstrap process
    2. Database: Initialize the database system and register autoload functions
    3. Variables: Load system variables and all enabled bootstrap modules
    4. Session: Initialize session handling
    5. Page header: Invoke `hook_boot()`, init the locking system and send
       default HTTP headers
    6. Language: Init all the defined language types
    7. Full: Final phase, Drupal is fully loaded now, this phase validates and
       fixes the input data

Example of how requests are routed to modules:

    URL: http://example.com/q=node/3
    Maps to:
        Module: `node.module`
        Function: `function node_page_view()`

There are 2 categories of modules:

1. core - come with Drupal
2. contributed - community created

### Info files (Drupal 7.x only)

- .info files are used for module metadata
- they are a Drupal extension of PHP built-in INI files
- The .info file should have the same name as the .module file and reside in the
  same directory.
    - For example, if your module is named example.module then your .info file
      should be named example.info.
- This file is in standard .ini file format, which defines properties in
  key/value pairs separated by an equals sign (key = value).
- Can wrap values in quotes. Quoted values can contain newlines
- `;` comments out the line but must be first on the line
- Details:
  https://www.drupal.org/docs/7/creating-custom-modules/writing-module-info-files-drupal-7x
- Has a predefined list of keynames

NB: when you change an info file you have to clear the cache for drupal to
notice

NB: Drupal 8 replaced .info files with .yml https://www.drupal.org/node/1935708

### Clearing the cache

- Option 1: Visit /admin/config/development/performance in your site admin and
  use the button
