//
//  BlogPostTests.m
//  Kata1Solution
//
//  Created by Eoin Kelly on 26/12/14.
//  Copyright (c) 2014 Eoin Kelly. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <XCTest/XCTest.h>
#import "BlogPost.h"

@interface BlogPostTests : XCTestCase

@end

@implementation BlogPostTests

- (void)setUp {
    [super setUp];
    // Put setup code here. This method is called before the invocation of each test method in the class.
}

- (void)tearDown {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
    [super tearDown];
}

- (void)testBlogPostExists {
    BlogPost *post = [[BlogPost alloc] init];
    XCTAssert([post isKindOfClass:[BlogPost class]]);
}

- (void)testBlogPostHasTitle {
    BlogPost *post = [[BlogPost alloc] init];
    NSString *title = @"the title";
    [post setTitle:title];
    XCTAssertEqualObjects([post title], title);
}

- (void)testBlogPostHasDate {
    BlogPost *post = [[BlogPost alloc] init];
    NSDate *today = [NSDate date];
    [post setDate:today];
    XCTAssertEqualObjects([post date], today);
}

- (void)testBlogPostHasBody {
    BlogPost *post = [[BlogPost alloc] init];
    NSString *body = @"The post body";
    [post setBody:body];
    XCTAssertEqualObjects([post body], body);
}

- (void)testBlogPostPublishedStatusDefaultsToFalse {
    BlogPost *post = [[BlogPost alloc] init];
    XCTAssertEqual([post isPublished], FALSE);
}

- (void)testBlogPostHasPublishedInfo {
    BlogPost *post = [[BlogPost alloc] init];
    [post markAsPublished];
    XCTAssertEqual([post isPublished], TRUE);
}

- (void)testBlogPostCreationDateDefaultsToReferenceDate {
    BlogPost *post = [[BlogPost alloc] init];
    NSDate *refDate = [NSDate dateWithTimeIntervalSinceReferenceDate:0];
    XCTAssertEqualObjects([post creationDate], refDate);
}

- (void)testBlogPostCreationDateCanBeSetViaConvenienceInitializer {
    NSDate *now = [NSDate date];
    BlogPost *post = [[BlogPost alloc] initWithCreationDate:now];
    XCTAssertEqualObjects([post creationDate], now);
}

// TODO: this just tests that there is no default named setter - is there a better
//       check that the value is read-only?
// TODO: turn off compiler warning ???
- (void)testBlogPostCreationDateDoesNotHaveASetter {
    XCTAssertFalse([BlogPost instancesRespondToSelector:@selector(setCreationDate)]);
}

- (void)testBlogPostHasAuthors {
    BlogPost *post = [[BlogPost alloc] init];
    NSArray *authors = @[ @"Bryron", @"Shelly" ];
    [post setAuthors:authors];
    XCTAssertEqualObjects([post authors], authors);
}

- (void)testBlogPostMakesACopyOfAuthors {
    BlogPost *post = [[BlogPost alloc] init];
    NSArray *authors = @[ @"Bryron", @"Shelly" ];
    [post setAuthors:authors];
    // TODO:
    // post.authors should be a different object to authors (but have same contents)
    // can I get the reference count for objects ???
}

- (void)testBlogPostTitleAsUppercase {
    BlogPost *post = [[BlogPost alloc] init];
    [post setTitle:@"hello"];
    XCTAssertEqualObjects([post titleAsUppercase], @"HELLO");
}

- (void)testBlogPostContentLength {
    // TODO: how to test a private method?
    BlogPost *post = [[BlogPost alloc] init];
    post.title = @"hello";
    post.body = @"there";
    // NSLog(@"%@", [post performSelector:@selector(contentLength)]);

    // XCTAssertEqual([post performSelector:@selector(contentLength)], @(10));
}
@end
