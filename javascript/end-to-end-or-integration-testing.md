# Integration testing in JS

There are 2 families

1. test sits on the page with the app
2. test sits in node/browser and remote controls another browser

they are differtiated by where the test lives relative to the app

# Options

The options are:

1. remote-control a browser via the WebDriver protocol
2. hook in at the JS framework level (if any)
    * -- only works for frameworks that support it
    * -- different for each framework
    * ?? AFAIK ember is the only framework to do this
    * ember runs itself in a div on a page surrounded by the test harness HTML
        * the ember test helpers (visit, andThen, find etc.) let you manipulate
          the active route and read and change the DOM under the ember root
        * the ember assertion helpers let you inspect the state of your ember
          app to make assertions about it at various points
        * ember runs its integration tests on page rather than trying to remote control a browser - pros/cons:
            * does not use webdriver
            * ++ no java server required
            * ?? can interactions that open new windows/popups be tested e.g. a facebook login
            * ++ faster than using selenium because you are not booting a whole new browser process
            * ?? how does it work when testing multiple browsers?
            * embers test.html loads testem javascript as well as the ember app
        * so in a way the ember app is remote-controlling itself and everything is just running in the browser
3. testem
    * is basically just a runner that will re-run things when some files change
    * knows how to run 2 types of thing:
        1. browsers
            * it creates a HTTP server and sends the browser to it. Form there
              it injects an iframe on the page and communicates back to the
              server via websockets
            * it has special knowledge of where to find the browser process on your machine (as it knows your architecture)
            * does not autolanuch browsers unless you tell it to
        2. normal processes
            * it captures STDOUT of the process
            * => it can be a runner for any process you want e.g. test ruby, shellscripts etc.
    * how testem runs browser tests:
        * when you run testem, it sets up a HTTP server at `http://localhost:7357/`
        * when you hit that server with a browser it redirect you to whatever url you configured e.g. `/tests/index.html?hidepassed%`
        * testem intercepts console output and window.onerror and sends those to its server via a websockets connection
        * The "TEST'EM SCRIPTS" thing you see on the page is from the testem iframe
        * The client-side script that reports results back to the Testem server via Socket.IO.
        * It also restarts the tests by refreshing the page when instructed by the server to do so.
        * it loads /testem/connection.html into an iframe
        * things the testem iframe does:
            ```js
            takeOverConsole()
            interceptWindowOnError()
            initTestFrameworkHooks(Testem)
            setupTestStats()
            ```
    * it can function as a proxy for API calls from your app to an API server on the Internet
    * it can run preprocessors on your files before running the tests
4. Karma
    * seems to be very similar architecture to testem
    * does not seem to be super active recently
    * angular team recommend karma for running unit tests and protractor for running e2e tests
        * why? what are the advantages of the selenium method

### testem pros/cons vs webdriver

* -1 testem: testem can't run the same tests locally and on a remote machine
* +1 testem: testem doesn't require any special browser driver
* ?? how does it handle multiple window tests e.g. a facebook login
* ?? testem might be faster than webdriver?
* +1 testem: it is much easier/acutally possible to pause a test in the middle and inspect what is happening
* +1 testem: it allows you to see your console.log output in the terminal which makes debugging easier. if you run selenium locally in a real browser (not phantom) you can pause the code and use the dev tools to debug stuff
* +1 selenum: your tests run in node (or ruby), not the browser which can be an easier dev experience
* -1 testem: selenium enforces more isolation between your test and the app - it is easier to cheat with testem
* Tests are different in tone: testem runs tests beside your app in a browser, webdriver tests control the browser to test your app
    * In testem tests, your POV is "I am on the page _with_ the app - I can control it and read info from it"
        * this is a similar POV that we are used to from in-browser testing frameworks like jasmine/qunit
    * In webdriver tests, your POV is "I am using HTTP commands to
      remote-control the browser - i send commands and check responses to make
      assertions"



## options for selenium client

* https://www.npmjs.com/package/selenium-webdriver
    * official selenium JS driver
    * good reference docs, no great tutorials
    * ++ does not need a selenium server when running locally - it runs and talks directly to the drivers
        * needs a selenium server when connecting to a browser on a remote host
        * ?? does this make it quicker than the alternatives when running locally
    * ++ uses promises for everything
* webdriver.io
    * controls a selenium server via the JSON wire protocol
    * no assertion framework built-in
    * -- puts the commands you give it on a a "command queue" that gets
      executed asynchonrously -  this makes is harder to share data between the
      callbacks
    * -- no promises API, callbacks only. makes tests very nested
    * predates the official WebDriverJS client (webdriver used to be called webdriverjs prior to 2.0)
    * talks webdriver HTTP protocol to the java server
* nightwatch
    * controls a selenium server via the HTTP protocol
    * has an assertion framework built-in
    * will start selenium server for you
    * has a test runner built-in e.g. does berforeEach etc.
    * since end of 2013
    * talks webdriver-wire-protocol directly
* protractor
    * controls a selenium server
    * does not provide a test framework - can integrate with others
    * -- they focus very much on angular - it may work but they make no
      garuantees (https://github.com/angular/protractor/issues/1507)
    * a wrapper around WebDriverJS
* Intern
    * seems very complete/very huge
    * also provides unit testing
    * returns promises for everything
    * built on the webdriver standard i.e. uses selenium
    * > Native mobile application UIs can be tested by Intern using an Appium, ios-driver, or Selendroid
    * uses AMD modules
    * uses selenium
* Casper
    * -- phantomjs only
* Capybara
    * in ruby
    * -- async waits are really tricky


A key question for the "selenium family" of testing is which libary is the easiest to work with
    * is testing JS with ruby an ok idea? that q boils down to "how does capybara compare to the JS options as a testing lib?"

## Cloud based selenium servers

* BrowserStack
* Sauce Labs
* TestingBot

## Ways of using selenium server from JS

* Official way is WebDriverJS https://github.com/SeleniumHQ/selenium/wiki/WebDriverJs
* is `selenium-webdriver` on npm
* has `/remote` module for controlling the standalone selenium server
* uses promises

* the wire protocol used between selenium server and client is specified by W3C now


* all the selenium based solutions rely on polling to know how long to wait for async operations

challenges for these things

* how does it handle popup windows like how FB auth works in browser
* how does it handle asynchonous operations or operations that take a "long" time

Aside: sources of async in the browser

* XHR
* setTimeout
* setInterval
* IndexedDB
* DOMMutationObservers
* postMessage
* ??? others

90% of async in ember apps is XHR, setTimout, run loop



# Selenium architecture

WebDriver is a compact Object Oriented API
the `selenium-webdriver` gem/npm package provides an OO API for controlling the webdriver


## JSON Wire protocol

* the protocol spoken between "selenium-client" and "selenium-server"
* https://code.google.com/p/selenium/wiki/JsonWireProtocol
* a RESTful web service providing JSON over HTTP
* In selenium protocol terminology "session" and "browser" are 1:1
* There are ~100 commands in the wire protocol
* Protocol command reference: https://code.google.com/p/selenium/wiki/JsonWireProtocol#Command_Reference

## "selenium-http" clients

* The various `selenium-webdriver` gem/npm packages etc. are basically fancy
  HTTP clients
* they convert the OO commands our code gives them into the appropriate
    HTTP req for an endpoint (which can be driver or server)
* all they need for configuration to communicate with the server is its
    host and port number

### selenium docs

* http://selenium.googlecode.com/git/docs/api/javascript/index.html (old docs)
* https://seleniumhq.github.io/docs/index.html (new docs)
* https://code.google.com/p/selenium/wiki/JsonWireProtocol

## "selenium-http" servers

* chromedriver
    * source code for it lives in the chromium source tree
    * a separate executable
    * functions as a sort of server process that takes care of launching and controlling the chrome processes you need
    * when run it listens on localhost:9515 and will respond to HTTP requests sent to that port
* firefox itself
    * I *think* firefox can be run in such a way that it behaves as a selenium server
* a safari extension
* phantomjs
    * `phantomjs --webdriver=PORT` will run phantomjs as a selenium server (since 1.8+)
* the selenium java server
    * can be used as a proxy to pass on requests and responses to other servers
    * situations when you *need* the java server
        1. the "selenium-server" is on a remote host
        2. you want to send commands to more than one browser at a time - this
           is why many testing frameworks require it


# approaches for testing the pack

* would be good to be as alike to our ember apps as possible
* how much setup would be required

* +1 selenium: no changes to index.html required. testem would require a
  test.html that loads the app + other stuff that would need to be updated
  whenever index.html is
* +1 testem: we can stop and debug a bit more easily. todo: figure out how to pause a selenium test and debug
* ?? which one makes multiple windows (e.g. facebook) easier to manage
* +1 selenium: having the tests run in node means I can use my facebook helpers stuff which only runs in node.
