# UIImage

* UIImage objects are immutable
    * you need to use an NSData object to get access to the underlying data e.g. using the  UIImageJPEGRepresentation function
    * automatically purges the image data from cache if device is low on memory. It will automatically reload it if you ask for the image again but there will be a perf cost.
UIImage(imageNamed:)
* loads data from the "system cache" if it exists in there
* loads image from asset catalog _or_ main application bundle
* if the device screen has a scale other than 1.0 e.g. 2.0 this method will look for the given name with an "@2x" suffix on it first and fall ack to the given name if it fails to find that e.g. "button" will result in a search for "button@2x" and then "button"

```swift
UIImage(named: "somename.png") // returns image object or nil
UIImage(named: "some_asset_catalog_name") // returns image object or nil
```

