
Tools

* https://jakearchibald.github.io/svgomg/
    * web gui to a node SVG optimization tool, lets you see your changes live so is superior to the cmd line version

Resources

* http://svgtutorial.com/
* https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial
* http://svgtutorial.com/additional-tools-and-resources/

Basics

* An XML dialect for describing 2D artwork
* the artwork can be manipulated by CSS and JS (SVG has a "DOM")
    * only if SVG is inline in HTML or loaded via `<object`>
    * `<img src="foo.svg" />` cannot be manipulated by CSS or JS
* artwork is vector art
* shapes etc. are laid out on a 2D coordinate system (x,y)
* reasons to use it
    * artwork scales well to different resolutions
    * artwork may be smaller than equivalent bitmap image (depends on artwork)
    * artwork can be modified on page by CSS and JS
        * CSS animations
        * JS animations
* current version of spec is SVG 1.1 (2011) - https://www.w3.org/TR/SVG/
* browser support: everything except IE8 and older, Android 4.3 and older
* if you convert bitmap to SVG it will just base64 encode the bytes - waste of your time and bandwidth
* units of measure
    * in SVG code, units default to `px` if you don't specify them
    * you can use all the usual units of measure: px, em, pt, in, cm, mm, pc, ex
* > the SVG that Adobe Illustrator gives you isn't particularly optimized. It has a DOCTYPE and generator notes and all that junk

## Code

* elements - short reference at http://svgtutorial.com/important-svg-elements/
    * `<svg></svg>`
        * container element
    * `<g></g>`
        * create groups
        * you can apply transforms, fills etc to the whole group
    * `<path />`
    * `<rect />`
    * `<circle />`
    * `<line />`
    * `<polygon />`
    * `<text />`
    * `<textPath />`
    * `<stop />`
    * `<linearGradient></linearGradient>`
    * ``
    * ``
* attributes
    * `viewBox="x y width height"`
        * applied only to `<svg>` element
        * allows you to zoom and crop the element
        * allows you to adjust the viewport of your SVG

## Guidelines for creating SVG in AI

* fill the artboard - ensures SVG will fill the space given to it by the web dev on the page
* convert to outlines (removes dependency on external assets like fonts)
* Use special "Save as SVG" export option
