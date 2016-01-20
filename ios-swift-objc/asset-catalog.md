# Asset catalog

    https://developer.apple.com/library/ios/recipes/xcode_help-image_catalog-1.0/_index.html
    https://developer.apple.com/library/ios/documentation/Xcode/Reference/xcode_ref-Asset_Catalog_Format/index.html

* "assets" are an abstraction over "things (images etc.) that need to use different versions of themselves depending on
    * device RAM size
    * device screen size and zoom
* assets allow you to refer to a single unique name for each asset in the code
  and then the system takes care of mapping that to the correct file on disk
* An Asset catalog contains Sets
* Each Set has attributes
* For a single or group of assets you can specify
    * which platforms this asset should be prefered on
    * whether the asset(s) should be prefered on devices with different amounts of memory
* Each asset catalog in Xcode has a corresponding folder in the Finder, with an .xcassets extension
* It has a subfolder for each set which contains
    * its image representations
    * a `Contents.json` file (describes each image)
        * is automatically created by Xcode.

## Asset catalogs can contain 9 kinds of thing

1. Data sets
    * used for any type of data file _except_ a binary executable
    * this does not seem to be super useful except to be neat and tidy and have
      all assets in one place
2. Folders
    * used to group assets or other folders
    * GOTCHA: they use the same yellow icons as "groups" do in the project
      navigator BUT they are reflected on the file system (unlike groups)
3. Launch images
    * Launch image sets contain all the representations of the static launch
      image that iOS apps must provide to be temporarily displayed when the app
      is launched.
4. iOS App icon set
    * App icon sets contain all the representations of the icon that iOS apps
      must provide to be displayed on a device’s Home screen and in the App
      Store.
5. OSX icons
    * contains all the representations of an OSX icon that the app must provide
6. Sprite Atlas
    * a texture atlas used with `SKTextureAtlas`
7. Watch complication placeholder images
8. watchOS app icons
    * "App icon set" contains all the representations of an icon needed for the watch
9. Image set
    * an image set contains all the versions, or representations, of an image
      that are necessary to support various devices and scale factors.
    * Image sets are used to group different resolutions of the same image

## How asset catalogs are stored on filesystem

In Xcode:

```
`Images.xcassets`
    AppIcon
    LaunchImage
```

is actually on the filesystem as:

```
Images.xcassets
    AppIcon.appiconset/
      Contents.json
    LaunchImage.launchimage/
      Contents.json
```

## How to use asset catalog in code

* each set in the catalog has a name that can be used instead of an image filename in UIKit methods that load images
* Your Xcode project can have more than one asset catalog but they are all boiled down to a single namespace for use by code
* only the set names are exposed to code so they need to be unique across your asset catalogs
    * it kind of implies there is no benefit to having multiple asset catalogs in a project ???
* You still have to import the images into your project - the asset catalog just
  provides the link between the image and what is is used for in code (rather
  than having the image file name do that)

Advantages:

* lets you refer to the images from code using the catalog name (not file path)
    * methods that can use asset catalog names:
        * UIImage:imageNamed:
* lets you avoid the `foo@2x.png` naming convention - you can name your images
  anything you want
* on iOS 7+ deployment targets XCode compiles the asset catalogs into a runtime
  binary format that makes it faster to load.
