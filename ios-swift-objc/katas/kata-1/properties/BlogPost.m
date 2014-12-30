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
        _published = FALSE;
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

#pragma mark - Instance methods

- (NSString *)titleAsUppercase {
    return [_title uppercaseString];
}

- (void)markAsPublished {
    _published = TRUE;
}

#pragma mark - Private methods

- (NSNumber *)contentLength {
    return @([_title length] + [_body length]);
}
@end

