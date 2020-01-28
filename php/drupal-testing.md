# Drupal 8+ testing

Drupal core 7.x has legacy systeem called 'SimpleTest' which is being removed

## Unit tests

* no DB
* Mocking: prophecy
    * chips with phpunit
* `UnitTestCase` is the core class

## Kernel tests

* you have a DB
* only specific modules installed
* only specific entity types installed
* only specified configuration and schemas installed
* descends from `KernelTestCase`

## Function Tests

* does a full "Testing" profile install
* this is what SimpleTest was (the only kind of test available in Drupal 7)
* can GET POST requests
* Installs all configurations and schemas
* installs module dependencies
* Inherits from `BrowserTestCase`

## Javascript Functional tests

* identical to functional tests but uses WebDriver (as of 8.5) to test JS interactions (it used to be Phantomjs)
* inherits from `JavascriptTestCase`

## Behat

* tests the actual site install (with all your admin changes)
* you point behat at your "test server" and run behat against it
    * have a process that clones prod data and sanitizes it
* config dependent scenarios
* behat is currently the best way do do a full end to end test of a drupal site
* https://www.drupal.org/project/drupal/issues/2793445 would allow `BrowserTestCase` and `JavascriptTestCase` to be run against a real site which would mean you don't need behat
    * but it hasn't landed yet so only exists as a patch you can add

## Code quoality tips

* raise an exception if you encounter a problem
* use asserts for things that should not be possible (they usually don't run in PHP in prod mode)
