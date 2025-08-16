Typhoeus seems the best choice here

https://github.com/typhoeus/typhoeus

> Built around libcurl and libcurl-multi to achive the best performance, is
> ready to process multiple simultaneous requests, cache responses. Built with
> FFI, therefore ready for use with any Ruby implementation

- wraps libcurl
- lets you make concurrent HTTP requests
- not a general concurrency solution
- faraday can wrap it
