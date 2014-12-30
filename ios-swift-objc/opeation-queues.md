iOS has a "main operation queue"

NSOperationQueue


```objc
// get a reference to the main operation queue of the app and add a block to it.
[NSOperationQueue mainQueue] addOperationWithBlock:myBlock];
```
