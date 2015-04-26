# Functional testing in JS

The options are:

* remote-control a browser via the WebDriver protocol
* hook in at the JS framework level (if any)
    * ember runs itself in a div on a page surrounded by the test harness HTML
        * the ember test helpers (visit, andThen, find etc. let you read and change the DOM under the ember root
        * the ember assertion helpers let you inspect the state of your ember app to make assertions about it at various points
        * ember runs its integration tests on page rather than trying to remote control a browser - pros/cons:
            * ?? does not use webdriver
            * ++ no java server required


options for remote-controlling a browser

* https://www.npmjs.com/package/selenium-webdriver
    * official selenium JS driver
    * lower level than webdriver or nightwatch by the looks of it
* webdriver.io
    * controls a selenium server via the JSON wire protocol
    * no assertion framework built-in
    * -- puts the commands you give it on a a "command queue" that gets executed asynchonrously -  this makes is harder to share data between the callbacks
* nightwatch
    * controls a selenium server via the JSON wire protocol
    * has an assertion framework built-in
    * will start selenium server for you
    * has a test runner built-in e.g. does berforeEach etc.
* protractor
    * controls a selenium server via the JSON wire protocol
    * does not provide a test framework - can integrate with others
* Intern
    * seems very complete/very huge
    * also provides unit testing
    * returns promises for everything
    * uses AMD modules
* Casper
    * phantomjs only


challenges for these things

* how does it handle popup windows like how FB auth works in browser
* how does it handle asynchonous operations or operations that take a "long" time
