# Big nerd ranch guide to iOS development

## Chap 1

* Interface builder is an "object editor"
    * creates instances of objects and manipulates their properties
* A `.storyboard` is an archive of object instances to be loaded into memory when necessary

## Chap 2

* notes are in the swift playground

## Chap 3

* Views are arranged in a heirarchy
* All views are instances of `UIView`
* Each instance of UIView has a "superview" property that points at its parent
    * UIWindow is the ultimate parent of all on-screen views
* `UIWindow` has `UIView` as a superclass
* `UIWindow` is the top of the view heirarchy on the screen (not the top of the heirarchy in the class sense)
* Each `UIView` renderers itself to a layer (`CALayer` instance)
    * layers are composited together to build the UI
* A view has a `subviews` property which is an array of child views
    * `subviews: [UIView]`

When you initialize a view in code you use `init(frame:)` designated initializer

* the `frame` specifies the view's
    1. size
    2. position relative to the superview
* => a view is ALWAYS a rectangle

```
// pseudo-code showing how the types relate to each other
CGSize {
    width: CGFloat // points not pixels
    height: CGFloat // points not pixels
}

CGPoint {
    x: CGFloat // points not pixels
    y: CGFloat // points not pixels
}

CGRect {
    origin: CGPoint
    size: CGSize
}

UIView {
    var frame: CGRect
}


// so you can do the following
someView.frame.origin.x
someView.frame.origin.y
someView.frame.size.width
someView.frame.size.height
```

* When the application is launched, the view for the "initial view controller" is added to the root level window
    * this defaults to `ViewController.swift`
        * TODO: how is this chosen? I can't find a setting in xcode? is it based on filename?
* each instance of `ViewController` has a view property
    * the view property for the "initial view controller" is the view that will be shown first
* The "initial view controller" has an associated view so any views you add are added as subviews of that
    * => the herirarchy is `UIWindow > ViewController.view > yourView`

### Auto-layout

Autolayout allows you to describe your layout in a way that the CGRect frame of
each view can be decided by the system at runtime so it can take into account
the screen resolution, size and orientation.

* the layout is based on the "alignment rectangle"
* The frame encompasses the entire view
* the alignment rectangle encompasses that part of the view you want to use for layout
* Frame and alignment rectangle are often the same but not always
* the _alignment rectangle_ is based on "layout attributes"

layout attributes:

* width|height
    * width or height of the "alignment rectangle" (NOT necessairly the view!)
* top|bottom|left|right
    * the spacing between the given edge of the alignment rectangle and the alignment rectangle of another view in the heirarchy
* centerX|centerY
    * these determine the center point of the alignment rectangle
* baseline
    * same as "bottom" for most but not all views e.g. UITextField has baseline high enough so that descenders on letters like g, y don't overflow the alignment rectangle
* leading|trailing
    * same as left|right but will map differently depending on whether language is ltr or rtl

* By default every view has an alignment rectangle and every view heirarchy uses auto-layout
* You cannot define a view's alignment rectangle directly - you define it indirectly through constraints
* HINT: As a general rule of thumb you need two constraints per dimension (horizontal and vertical) e.g.
    * in vertical
        * top + height
        * height + bottom

Nearest neighbour views are important to understand how auto-layout works

* nearest neighbour is the closest sibling view in the specified direction
* if a view does not have any sibling views in that direction then its nearest neighbour is its superview (container)

UI hints about layout problems

* blue constraints = everything is fully specified
* orange constraints = misplaced view (what you see in IB is different to what autolayout computed)
    * you can see an orange dotted line that shows where the view should be according to autolayout
    * can fix it via `Resolve Auto Layout Issues > Update frames`

UP TO END CHAP 3
