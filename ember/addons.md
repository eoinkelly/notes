An addon is an npm package. It may install bower packages but it _is_ the npm
package.

addons install bower by ??

addons can create generators

```
ember addon my-awesome-addon
# creates a dummy ember app in the test dir

/addon
    * everything exported in here is exported in the 'my-awesome-addon' namespace in the app
    * e.g. `/addon/foo.js` would be used as `import Thing from 'my-awesome-addon/foo';
/addon/index.js
/app
    * gets merged into your apps app/ tree
/public
/test-support
/vendor
    * gets merged into your apps vendor/ tree
/index.js
    * import files from your addon and re-export them to make the "public interface" of the addon
        * it prevents users from having to know about the structure of your addon/ dir
        * it means you can move files around within your addon wihtout breaking apps
```

You can add npm and bower packages to the project

You can add commands e.g. `ember dostuff` - this is used by deployment
environments e.g. a heroku addon might add a bunch of commands that heroku knows
how to run.
