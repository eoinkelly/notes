# Ember Components (Ember in Action Chapter 7)

Q: what is the right granularity for a component?
    * EIA book advocates for very small components with one job - very SRP

Q: what are the criteria to consider when deciding between a view and a component?
    * will this view be reused in different templates in your app? = medium for component
    * Will this view be reused across apps = strong  for component
    * Does this view require access to a lot of state from the app? = ???
    * Does this view need to exchange messages with other views/components in the app? = ???

Q: is there a performance penalty to compoents vs views?

Q: Is there a performance penalty to using lots of components?

Q: Should _everything_ be injected into a component?
  * Can it know about your route names? EIA seems ok with this - it hard codes route names in components

Q: At what point would you not use a component and use a view instead?

Q: How many injections can you have before component is the wrong thing?

Q: How do you avoid all the passing out of messages I had to do in polytech?

Q: How (if at all) do Ember components handle encapsulating CSS like web components will?

What is a component?
    * part of your app that you can reuse in _multiple_ places _without modification_.
        * multiple places!
        * without modification!

component = template + JS object
* the JS object has two roles
    1. view (it is the target for DOM events from template)
    2. controller (it is target for actions from template)

## Naming

* template name must begin with `components/` and also contain a `-` e.g. `components/my-awesome-thing`
* the dash is designed to prevent a name clash with the final web components spec
