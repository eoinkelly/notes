# React

- you have to use `className` instead of `class` in JSX
- `render()`
    - can only return one element (cannot return sibling elements)
    - you can work around it by wrapping in a
      `<React.Fragment></React.Fragment>` tag because that tag will not be
      rendered in the output
- JSX
    - `{}` in JSX means that we are opening a javascript context
        - `{ // i am comment }` is how you do comments in JSX
        - When passing in props you need to wrap any prop which is not a string
          in `{}`

## react-rails gem

It serialses the props you pass into the react component as JSON and then HTML
quotes them

e.g. when you use the `<%= react_component("compname", foo=..., bar=...) %>`
rails view helper it renders something like:

```
<div
    data-react-class="RespondentTable"
    data-react-props="{&quot;business_roles&quot;:[{&quot;id&quot;:1,&quot;name&quot;:&quot;Business Owner&quot;,&quot;group&quot;:&quot;Leadership&quot;,&quot;created_at&quot;:&quot;2018-03-04T15:26:23.862+13:00&quot;,&quot;updated_at&quot;:&quot;2018-03-04T15:26:23.862+13:00&quot;},{&quot;id&quot;:6,&quot;name&quot;:&quot;Employee&quot;,&quot;group&quot;:&quot;Operations&quot;,&quot;created_at&quot;:&quot;2018-03-04T15:26:23.888+13:00&quot;,&quot;updated_at&quot;:&quot;2018-03-04T15:26:23.888+13:00&quot;}],&quot;locations&quot;:[{&quot;id&quot;:3,&quot;name&quot;:&quot;Main office&quot;,&quot;address&quot;:&quot;123 Fake st&quot;,&quot;business_id&quot;:2,&quot;created_at&quot;:&quot;2018-03-13T11:28:38.100+13:00&quot;,&quot;updated_at&quot;:&quot;2018-03-13T11:28:38.100+13:00&quot;},{&quot;id&quot;:4,&quot;name&quot;:&quot;Factory &quot;,&quot;address&quot;:&quot;123 Factory st.&quot;,&quot;business_id&quot;:2,&quot;created_at&quot;:&quot;2018-03-13T11:28:38.102+13:00&quot;,&quot;updated_at&quot;:&quot;2018-03-13T11:28:38.102+13:00&quot;}]}"
>
</div>
```

When you compile react for production it seems to name all components `t` no
matter which component it is

## React dependencies

```
"devDependencies": {
    "concurrently": "3.5.1",
    "react-scripts": "1.1.1"
},
"dependencies": {
    "autoprefixer-stylus": "0.14.0",
    "firebase": "^4.10.1",
    "prop-types": "^15.6.0",
    "re-base": "3.2.2",
    "react": "^16.3.0-alpha.1",
    "react-dom": "^16.3.0-alpha.1",
    "react-router-dom": "^4.2.2",
    "react-transition-group": "^2.2.1",
    "serve": "^6.5.0",
    "stylus": "0.54.5"
}
```

- react-scripts
    - maintained by Facebook
    - part of _Create React App_
    - sets up a dev environment for React
        - does webpack & babel under the hood

Dependencies

- stylus
- autoprefixer-stylus
- prop-types
    - used to be part of React
    - lets you specify types for the properties you pass in
- firebase
    - hosted database
- re-base
    - lets us connect to firebase
- react
- react-dom
- react-router-dom
    - routing between URLs
- react-transition-group
    - animations
- serve
