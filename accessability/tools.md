
Options

* capybara-accessible
    * https://blog.pivotal.io/labs/labs/automated-accessibility-testing-rails
	* hits the google accessabilty API
	* wraps around the selenium capybara driver
	* hasn't been updated in 2 years
	* https://github.com/casecommons/capybara-accessible
	* ++ doesn't require you to write explicit "should be accessible test"
* https://github.com/bbc/bbc-a11y
    * checks a given set of URLs agains the BBC accessibility standards
    * creates a `a11y` command, functions a bit like the google `a11y` npm package except it checks BBC standards, not google ones
    * -- you have to manually specify each URL to hit
    * -- that means you'll have to so manual setup to get the app into a state there that URL can be tested e.g. logged in etc.
* http://khan.github.io/tota11y/
    * javascript thing that pops up on the page and lets you test accessibility
    * https://github.com/stevenspiel/tota11y-rails is a gem that enables it in rails
    * ++ seems pretty nice
    * ++ no testing overhead
    * -- needs to be explicitly turned on
    * could be turned on in development for manual checking and helping devs understand what they are making
    * ?? is it better than the google accessibility tools addon
* google's `a11y` npm package
* google's browser dev tools addon that does an a11y audit of the page
