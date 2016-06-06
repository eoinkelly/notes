# Selenium webdriver

    QUESTIONS:
    can i get firefox to open with the dev tools open
    why does binding.pry lock up firefox in an rspec test
    can i not kill the browser when the test finishes
        to allow setting up the browser for manual testing

* Webdriver is a W3C spec
    * https://w3c.github.io/webdriver/webdriver-spec.html
    * lots of the terminology e.g "session", comes from the spec
* Selenium existed before the WebDriver spec
    * but it now includes an implementation of that spec in Selenium version 2
* Selenium webdriver docs
    * http://docs.seleniumhq.org/docs/03_webdriver.jsp
* Each browser has a driver


* Gem docs
    * https://github.com/SeleniumHQ/selenium/wiki/Ruby-Bindings
* Gem source:
    * https://github.com/SeleniumHQ/selenium/tree/master/rb

Objects

Driver
    * usually a single instance of a driver that represents the connection to the browser
WebElement
    * the driver provides "finders" which return instances of web elements

You can find elements by

1. CSS
    * uses built-in browser CSS selector support
1. name attribute
1. id attribute
1. class attribute
1. link text
1. partial link text
1. xpath
