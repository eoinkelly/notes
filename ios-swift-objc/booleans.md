There are a number of values that evaluate falsy

* FALSE
* false
* 0
* NO

And to truthy:

* true
* TRUE
* 1
* YES

* An uninitialized boolean defaults to FALSE

What is diff between the types?
    BOOL
    bool
    Boolean
    Boolean_t


```objc

- (void)testBooleans {
    Boolean f;
    Boolean t = true;

    XCTAssertEqual(f, false);
    XCTAssertEqual(f, FALSE);
    XCTAssertEqual(f, NO);
    XCTAssertEqual(f, 0);

    XCTAssertEqual(t, true);
    XCTAssertEqual(t, TRUE);
    XCTAssertEqual(t, YES);
    XCTAssertEqual(t, 1);
}
```
