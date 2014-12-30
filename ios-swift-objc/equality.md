# Equality in ObjC

# ==

* test equality between two primitve C objects
* tests whether two pointers are pointing at the same object

QUESTION: == seems to work right for NSString and NSArray too - how does this work?
QUESTION:

# isEqual

* comes from NSObject
* can and should be overridden in child classes

# Testing hash keys for equality

* You can use any object as a key provided it implements `hash` and `isEqual`. Consequences:
    * `NSString` is a good choice:
        * `hash` and `isEqual` is based on characters in string
    * `NSObject` is a bad choice:
        * pointer is the hash and == is the isEqual
        * any subclasses of NSObject I create are also poor

```objc
#import <UIKit/UIKit.h>
#import <XCTest/XCTest.h>

@interface EqualityTests : XCTestCase

@end

@implementation EqualityTests

- (void)setUp {
    [super setUp];
}

- (void)tearDown {
    [super tearDown];
}

// Not using XCTest* as I want to be explicit

- (void)testEqualityOfScalars {
    int a = 1;
    int b = 1;

    if (a == b) {
        NSLog(@"ints are equal");
    } else {
        XCTFail(@"Fail!");
    }
}

- (void)testEqualityOfObjects {
    NSString *a = @"foo";
    NSString *b = @"foo";

    // == seems to work on NSString instances
    if (a == b) {
        NSLog(@"Strings are equal");
    } else {
        XCTFail(@"Fail!");
    }

    // isEqual works on NSString instances
    if ([a isEqual:b]) {
        NSLog(@"String objects are equal");
    } else {
        XCTFail(@"Fail!");
    }

    NSArray *as = @[@"a", @"b"];
    NSArray *bs = @[@"a", @"b"];

    // isEqual works on NSArray instances
    if ([as isEqual:bs]) {
        NSLog(@"Array objects are equal");
    } else {
        XCTFail(@"Fail!");
    }

    // == seems to work in NSArray instances
    if (a == b) {
        NSLog(@"Array pointers are equal");
    } else {
        XCTFail(@"Fail!");
    }
}

@end
```
