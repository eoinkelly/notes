# Managing state in React

## Sources

* http://andrewhfarmer.com/react-ajax-best-practices/
* https://medium.com/@dan_abramov/smart-and-dumb-components-7ca2f9a7c7d0#.2u3ofv1zg
* https://twitter.com/ryanflorence/status/743482899211575297
	```
	Spreadsheet? MobX
    Persistent relational data? Relay
    Normalizing data from multiple endpoints? Redux
    Everything else: component state
    ```

## Options for state mangement

1. root component does it all
1. container components do it
1. flux does it
1. flux-saga does it
1. mobx does it

I'm not considering relay here because moving to graphql is not an option right now


## react-rails contstraints

Using react-rails with the following assumptions

* there will likely be multiple "react roots" on the page (the page is still mostly server side
* go easy on the fancy es6 stuff as a lot of the team don't do JS fulltime

## A sensible growth plan seems to be

For each "react root" on the page:

1. Start with a root component that manages all state
1. refactor into container components (which can manage state) when you see opportunites
1. refactor into flux if state management becomes hairy using container components

Component state is still used even with flux/mobx/whatever

flux & friends are for when multiple react components need to work with the same set of data (and maybe persist it to server(s) or at least different endpoints)

---------------
What if multiple react roots need to share state?

* options:
    * move directly to flux, inject the flux store into all react roots
    * ???
---------------
