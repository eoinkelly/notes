//
//  BlogPost.m
//  Kata1Solution
//
//  Created by Eoin Kelly on 26/12/14.
//  Copyright (c) 2014 Eoin Kelly. All rights reserved.
//

#import "BlogPost.h"

// create class extension
@interface BlogPost ()

// declare private methods
- (NSNumber *)contentLength;

@end

@implementation BlogPost

#pragma mark - Initializers

- (instancetype) init {
    self = [super init];

    if (self) {
        _isPublished = FALSE;
        _creationDate = [[NSDate alloc] initWithTimeIntervalSinceReferenceDate:0];
    }

    return self;
}

- (instancetype)initWithCreationDate:(NSDate *)date {
    self = [self init];

    if (self) {
        _creationDate = date;
    }

    return self;
}

#pragma mark - Getters and setters

- (NSString *)title {
    return _title;
}

- (void)setTitle:newTitle {
    _title = newTitle;
}

- (NSDate *)date {
    return _date;
}

- (void)setDate:newDate {
    _date = newDate;
}

- (NSString *)body {
    return _body;
}

- (void)setBody:newBody {
    _body = newBody;
}

- (BOOL)isPublished {
    return _isPublished;
}

- (void)markAsPublished {
    _isPublished = TRUE;
}

- (NSDate *)creationDate {
    return _creationDate;
}

- (NSArray *)authors {
    return _authors;
}

- (void)setAuthors:(NSArray *)newAuthors {
    _authors = [newAuthors copy];
}

#pragma mark - Instance methods

- (NSString *)titleAsUppercase {
    return [_title uppercaseString];
}

#pragma mark - Private methods

- (NSNumber *)contentLength {
    return @([_title length] + [_body length]);
}
@end

