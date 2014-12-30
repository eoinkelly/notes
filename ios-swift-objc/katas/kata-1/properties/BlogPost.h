
//
//  BlogPost.h
//  Kata1Solution
//
//  Created by Eoin Kelly on 26/12/14.
//  Copyright (c) 2014 Eoin Kelly. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface BlogPost : NSObject

@property (nonatomic, copy) NSString *title;
@property (nonatomic, strong) NSDate *date;
@property (nonatomic, copy) NSString *body;
@property (assign, readonly, getter=isPublished) BOOL published;
@property (nonatomic, strong, readonly) NSDate *creationDate;
@property (nonatomic, copy) NSArray *authors;

- (void)markAsPublished;
- (NSString *)titleAsUppercase;

- (instancetype)init NS_DESIGNATED_INITIALIZER;
- (instancetype)initWithCreationDate:(NSDate *)date;

@end

