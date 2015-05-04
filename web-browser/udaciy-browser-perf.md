# Udacity google performance course

to get 60fps we need to render a new frame every 16ms but browser housekeeping means we need to complete our work in ~10ms-12ms


How the browser renders a single frame

1. something triggers a visual change on the page e.g. JS or CSS animation
2. "recalculate styles" (tooling name) = combine DOM and CSS trees to make render tree
    * render tree is like DOM but only contains _visible_ elements e.g. no `<head>` or anything with a style of `display: none;`.
    * NOTE: things that are not in viewport or have 0 height are still in the render tree
3. "layout" (tooling name) (also called "reflow")
    * figure out how much space each element in the render tree needs
    * when layout changes it changes for the whole document i.e. its scope is "whole document"
        * any exceptions to this?
        * you can limit the scope of a layout with a "layout boundary"
            * http://wilsonpage.co.uk/introducing-layout-boundaries/ <-- TODO
4. "paint" (tooling name)
    * turns the vector set of instructions into raster
    * also includes "image decode and resize" which turns JPEG/PNG into bitmap for painting on the screen
5. "composite layers"
    * composite the layers together
    * layers are actually painted in square tiles
    * these tiles are created on the CPU and then uploaded to the GPU



The rendering pipeline looks like:

```
JS > Style > Layout > Paint > Composite
```

JS is the "trigger a visual change" thing and could also be a CSS animation or a web animation API

but the full pipeline is not always used

```
# scenario 1: our "visual change" touches a "layout property"
# => full pipeline used
JS > Style > Layout > Paint > Composite

# scenario 2: our "visual change" touches a "paint only property" e.g. "background"
JS > Style > Paint > Composite

# scenario 3: our "visual change" only changes the arrangment of layers
JS > Style > Composite
```

Aside: Flexbox is created in CSS but it does not cause "recalculate styles" when the layout changes, presumably becasue the style has not actually changed, just the layout on the page

* http://csstriggers.com/ has a good summary of which CSS properties require which parts of the pipeline
    * note that transform and translate things only require compositing so are the most performant


When perf matters most: RAIL
4 phases of an app from a perf POV

1. Response
    * needs to happen within 100ms to not be noticible
1. Animation
    * can be things that need to stick to the users finger or transitions and animations
    * need to provide a new frame every 16ms
1. Idle
     * do work you have defered during the other phases
     * keep this work to 50ms chunks so that the app can still respond to an input within 100ms
1. Load
    * ideally happens within 1s
