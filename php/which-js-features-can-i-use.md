# How to tell which JS features you can use in a project

## You are using Typescript

* If you are using only Typescript then your TS version tells you what features you have
* Otherwise if your TS is being processed later by babel (which is how webpacker does it for example) then the babel advice below applies too.
* Be aware that TS gets first crack at your code so it'll matter more than babel in many cases e.g. if you use decorators then you need to use the TS ones, not whatever babel recognises

## You are using babel

Use this to determine which version of babel/core and which babel plugins you have installed:

```bash
yarn list --pattern babel

# TODO equivalent npm
```

Where babel stuff comes from:

1. If you use webpacker, your version of webpacker matters most because that pulls in `babel/core` and `babel/preset-env`
1. your version of `@babel/core` matters
1. your version of `@babel/preset-env` matters
1. your config for `@babel/preset-env` matters
    * config can be in
        * `package.json` (default for us)
        * `.browserlistrc` file
        * baked into `babel.config.js`
1. any babel plugins you have installed in **your app's** package.json


## Aside: Browserlist

My current default `browserslist` (set in rails-template):

```jsonc
// package.json
 "browserslist": [
   "defaults", // de-sugars to: > 0.5%, last 2 versions, Firefox ESR, not dead
   "not IE 11",
   "not IE_Mob 11"
 ],
 ```
