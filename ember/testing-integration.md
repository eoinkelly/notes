
How to handle fixture data

Options
1. Maintain a single set of test fixtures
    * switch adapter based on environment
    * -- all tests depend on these fixtures so they are very hard to change
1. Maintain multiple sets of test fixtures
    * -- probably a lot of duplication between each set
1. Stub out HTTP calls so that the app thinks it is talking to a server
    * options
        * https://github.com/trek/pretender
    * ++ you only make as much "fixture" as you need for each test and that fixture data is right there above the test
