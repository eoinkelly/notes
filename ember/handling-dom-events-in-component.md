# What is the best way of handling click events in my component?

option 1: filter everything through click() handler

- click runs a test on the event object to decide which element triggered the
  event and therefore what to do
-   - click-handler has to have a lot of knowledge of the DOM structure
- = can invoke various helper functions to do the work

option 2: send things to both click handler and an action handler

-   - the logic for what happens in response to an action is split => could be
      harder to understand
-   - it would let you handle the logic that updates the state of the app and
      the logic that updates the DOM separately
- = can invoke various helper functions to do the work

option 3: trigger actions only

- send a param to the action that lets the JS create a selector to find the DOM
  element that sent it.
-   - not all actions will need to know who triggered it.
-   - action names are more semantic than 'click' - easier to split logic
- = can invoke various helper functions to do the work

option 4: create new view object to handle each type of DOM element

- let those views take care of updating that element and have them pass up
  logical actions to the parent view
-   - logic is split across objects ? bad?

option 5: multiple handlers that fire on click via `function () {}.on('click')`

- ???

reading: https://github.com/emberjs/ember.js/issues/1684
https://github.com/emberjs/ember.js/issues/1986
