
Some handy-dandy preprocessor macros

```objc

// QUESTION: why is this wrapped in a test for __OBJC__ ???
    perhaps it lets it be included in C/C++ files too ???

#ifdef __OBJC__
    // Handy little debugging macros
    #ifdef DEBUG
    #   define DLog(fmt, ...) NSLog((@"%s [Line %d] " fmt), __PRETTY_FUNCTION__, __LINE__, ##__VA_ARGS__);
    #else
    #   define DLog(...)
    #endif
    #ifdef DEBUG
    #   define ULog(fmt, ...)  { UIAlertView *alert = [[UIAlertView alloc] initWithTitle:[NSString stringWithFormat:@"%s\n [Line %d] ", __PRETTY_FUNCTION__, __LINE__] message:[NSString stringWithFormat:fmt, ##__VA_ARGS__]  delegate:nil cancelButtonTitle:@"Ok" otherButtonTitles:nil]; [alert show]; }
    #else
    #   define ULog(...)
    #endif

    // This only tests for iphone 5 and 5s *screen sizes* only - it is not TRUE for iPhone 6
    // Tests whether the screen height is equal (in so far as floats can be equal) to 568
    #define IS_IPHONE_5 ( fabs( ( double )[ [ UIScreen mainScreen ] bounds ].size.height - ( double )568 ) < DBL_EPSILON )
#endif
```
