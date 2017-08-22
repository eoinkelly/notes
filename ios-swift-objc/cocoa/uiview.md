# UIView

## Overview

* Every view you see on screen is a child of `UIView`
* each `UIView` instance has an array of children in `myView.subviews`
* The parent of all views on the screen is an instance of `UIWindow` (which is also a child of


There are two ideas of parent-child here

1. class inheritance e.g. UIWindow inherits from UIView
    * in this heirarchy UIView is the top parent and all other views inherit from it
    * it seems like most views inherit **directly** from UIView
        ```
        UIView > UIWindow
        UIView > UILabelView
        UIView > UIScrollView > UICollectionView
        UIView > UIScrollView > UITableView
        UIView > UICollectionReusableView
        UIView > UIImageView
        UIView > UILabel
        UIView > UIControl > UIButton
        UIView > UIControl > UITextField
        etc.
        ```
1. view heirarachy - which views have other views in their `subviews` array
    * in this tree the UIWindow instance is at the top


How drawing tot he screen works:

* Each view renders itself onto **its own** layer (instance of `CALayer`)
    * the layer is kind of like a bitmap image
    * note that the view knows how to render itself to the its layer
* then all the bitmaps (layers) are composited together to the screen


### Stacking

* UIViews stack in a fairly predictable way
* subviews which are later in the subviews collection stack higher
* child view stack higher than their parent views

I guess it's the CALayer objects which are actually stacked?

Aside: UILabel seems to do different things when stacking parent to child - if
both parent and child are UILabel then the parent hides the child for some
reason.

### Introspection

```
v = UIView(frame: CGRect(x: 0, y: 0, width: 100, height: 100))
v.backgroundColor = UIColor.red

v.superview // a link back to the view which contains this view on the screen

v.superclass // get parent class
v.subviews
v.addSubview(otherView)
```

### Debugging

```
myView.backgroundColor = UIColor.red
```

### Point dimension

* the dimension for origins and sizes in frames is point
* a pt is a relative unit of measure
* pt is consistent across devices of different pixel resolutions

### Frames

* each view instance has a `frame` property which is an instance of CGRect
* frame
    * controls view size
    * controls view position **relative to its parent view**
* the origin point is the top left point of the rectangle
* the origin distances are relative to the origin of the parent
* the origin of the UIWindow is the *very* top left of the screen i.e. your
  views will cover the clock and battery indicators if you position them at 0,0

```
// CGRect
//     CGSize
//         CGFloat
//         CGFloat
//     CGPoint
//         CGFloat
//         CGFloat

let r = CGRect(x: 0, y: 0, width: 100, height: 100)

print(type(of: r.origin))
print(r.origin) // :: CGPoint
print(r.origin.x) // :: CGFloat
print(r.origin.y) // :: CGFloat

print(type(of: r.size))
print(r.size) // :: CGSize
print(r.size.width) // :: CGFloat
print(r.size.height) // :: CGFloat
```


### Auto resize

* Views have an `autoresize` property which does ???

### UIWindow

* Every app has one window which displays the views on the iOS device
    * you can create other windows if there is a second screen
* the window has a single "root" view controller that contains all the other views you add
    * this property is held in the `myWindow.rootViewController` property
    * reason for this: when you want to replace all the content on the screen you just change one root view contoller for another
    * => the window instance is always "on screen"

### View controllers

```
NSObject > UIResponder > UIViewController
```

A group of views is managed by a UIViewController instance

* "managed" =>
    * updating the views when content changes
    * handling user input
    * resizing views and managing layout
* you usually have many view controllers each of which owns a part of the app's interface
* each view controller has one "root view" associated with it (available through `self.view`)
    * this view is visible in the view heirarchy
* Every app contains **at least one** custom subclass of UIViewController
* storyboards let you specify your views in a GUI and wire them to their view controller



### How does iOS know what to load first?

#### WIthout storyboards

* You set it up manually in the `application:didFinishLaunchingWithOptions` function
1. create a UIWindow instance
1. create a UIViewController instance to be the root view controller
1. assign the root view controller to your window
1. make the window visible

Example

```swift
import UIKit

@UIApplicationMain
class AppDelegate: UIResponder, UIApplicationDelegate {
    var window: UIWindow?

    func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplicationLaunchOptionsKey: Any]?) -> Bool {
        // create a root view controller (this will actually be the root of the
        // views under the navigation controller not under window)
        var rootViewController: UIViewController!

        // create a window instance an store it in our window property
        self.window = UIWindow(frame: UIScreen.main.bounds)

        // choose different root view controllers for iphone or ipad
        if UIDevice.current.userInterfaceIdiom == UIUserInterfaceIdiom.phone {
            rootViewController = ViewControllerPhone()
        } else {
            rootViewController = ViewControllerPad()
        }

        // create a navigation controller (manages a stack of view controllers) and pass the root controller we created into it
        let navigationController: UINavigationController = UINavigationController.init(rootViewController: rootViewController)

        navigationController.navigationBar.barStyle = UIBarStyle.black;
        navigationController.navigationBar.titleTextAttributes = [NSForegroundColorAttributeName: UIColor.white]
        navigationController.navigationBar.barTintColor = AppThemePrimaryColor;
        navigationController.navigationBar.isTranslucent = false


        // make the navigation controller be the root view controller
        self.window!.rootViewController = navigationController

        // show the window and make it the "key" window
        self.window!.makeKeyAndVisible()

        return true
    }

    // ... other stuff ...
}
```

#### With storyboards:

Summary: iOS loads a storyboard which in turn loads a custom subclass of UIViewController

* In XCode you set the "Main interface" in the buid settings to a storyboard file
* that storyboard file has a "View Controller Scene"
* that scene has a reference to a view controller swift file which is the one which is loaded for that storyboard
* aside: you can edit that relationship using the RHS column in XCode and choosing the "Identity inspector"


