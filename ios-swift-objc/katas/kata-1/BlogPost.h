//
//  BlogPost.h
//  Kata1Solution
//
//  Created by Eoin Kelly on 26/12/14.
//  Copyright (c) 2014 Eoin Kelly. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface BlogPost : NSObject
{
    NSString *_title;
    NSDate *_date;
    NSString *_body;
    BOOL _isPublished;
    NSDate *_creationDate;
    NSArray *_authors;
}

- (NSString *)title;
- (void)setTitle:(NSString *)newTitle;

- (NSDate *)date;
- (void)setDate:(NSDate *)newDate;

- (NSString *)body;
- (void)setBody:(NSString *)newBody;

- (BOOL)isPublished;
- (void)markAsPublished;

- (NSDate *)creationDate;

- (NSArray *)authors;
- (void)setAuthors:(NSArray *)newAuthors;

- (NSString *)titleAsUppercase;

- (instancetype)init NS_DESIGNATED_INITIALIZER;
- (instancetype)initWithCreationDate:(NSDate *)date;

@end
