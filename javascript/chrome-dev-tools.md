
## Sources panel

* dimmed JS properties => the property is not enumerable

### Folder color

* Blue
    * Regular folder, not contains any source maps files
* Orange
    * Folder is a "virtual" one created by reading sourcemap(s)

Resource color

* Yellow - JS
* Violet - CSS & Fonts
* Green - Images
* Grey - HTML


#### Orange folders

Orange folders are "virtual folders" created by interpreting the sourcemap.

Webpack creates a slightly "creative" but useful sourcemap where it

* puts all it's sourcemap files under the fake `webpack://` origin
* the `webpack:///webpack/bootstrap` is always included
    * it loads the rest of your JS
* when using webpack-dev-server
    * creates fake `(webpack)` folders to as the root dir of it's own stuff
    * creates fake `(webpack)-dev-server` folder to as the root dir of the webpack-dev-server client code
* creates the `.` folder as the root of all your client code
    * shows you all the node_modules stuff that gets included in your build too
