# History

Ember `{{#each}}` helper is an _extension_ of the handlebars one.

currently in ember there are 2 forms

1. context switching
2. non context swtiching

The handlebars block helpers are

- always context switching

    {{#each thing}} new context, only has access to thing {{/each}}

then ember extended the them with

    {{#each thing in things}}
        keeps context, the current iteration of things is in thing
    {{/each}}

Ember only documents its own "non context swtiching" versions and it calls them
"special forms" of the basic handlebars ones. There are currently two "special
forms":

```
{{#each post in posts}}
{{#with foos as foo}}
```

With the Ember 2.0 "block paramaters" syntax:

- any component or block helper will use the _same_ syntax
- not just #each and #with anymore

    <ui-calendar month={{currentMonth}} as |day|> </ui-calendar>

    {{#each things as |thing|}} {{/each}}

    {{#with things as |thing|}} {{/with}}

Block params will land in 1.12 at which point

```
{{#each post in posts}}
{{#with foos as foo}}
```

will be deprecated but not removed.
