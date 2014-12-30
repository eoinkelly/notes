# Tests

The testing framework reckons it can do unit and integration tests

* https://developer.apple.com/videos/wwdc/2013/?id=409

Tests are within their own bundle so to access the app bundle you use

[NSBundle bundleForClass:???]

* Tests are grouped into classes, each with their own setup & teardown
    * They are similar to RSpec `describe` blocks
* You have good control over what tests run in the test navigator
    * can show only failing tests


QUESTION: Is there any way to stub/mock/spy in ObjC?

QUESTION: ObjC seems to compare objects based on value not identity ???

```objc
- (void)testXCTNotionOfEquality {
    // * NSObject objects are equal if they have their properties have the same values
    BlogPost *p1 = [[BlogPost alloc] init];
    p1.title = @"hello";
    BlogPost *p2 = [[BlogPost alloc] init];
    p2.title = @"hello";
    XCTAssertNotEqualObjects(p1, p2);

    NSArray *a1 = @[ @"hi", @"there" ];
    NSArray *a2 = @[ @"hi", @"there" ];
    XCTAssertEqualObjects(a1, a2);
}
```
