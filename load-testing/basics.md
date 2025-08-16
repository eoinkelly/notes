# Load Testing

Sources

- http://guides.rubyonrails.org/v3.2.13/performance_testing.html
- http://work.stevegrossi.com/2015/02/07/load-testing-rails-apps-with-apache-bench-siege-and-jmeter/

## General principles

Types of testing

1. load testing
    - see how the system performs under specific hopefully real world conditions
2. performance testing
    - check the performance of the system under specific situations and compare
      the results over time
3. stress testing
    - overwhelm the system and see whether it breaks gracefully

- testing in dev isn't super useful as dev is rarely like production
- your test box must be close to the server box or network latency will be the
  bottleneck long before your server bottlenecks
    - https://www.sonassi.com/blog/magento-kb/why-siege-isnt-an-accurate-test-tool-for-magento-performance

## Tools

- jmeter
    - http://jmeter.apache.org/
    - seems to the most recommended tool
    - written in Java
    - supports many protocols, SMTP, TCP, others
    - has a GUI and command line
    - can be told to scan responses for CSRF tokens
    - definitely the most capable of the OSS tools
- apache bench (ab)
    - basic
    - -- doesn't let you hit different endpoints as part of a session
    - ++ simple
    - -- doesn't do groups of requests (flows)
    - -- doesn't let you pause between requests
    - -- https://stackoverflow.com/a/10264501 indicates its reporting isn't as
      good as jmeter
    - `ab -n NUM_REQUESTS_TO_MAKE -c CONCURRENCY http://example.com/foo`
    - ab timing results:
        - Connect is how long it took to establish a connection with the server
          (i.e. network latency).
        - Processing is the amount of time between sending the first byte of the
          request and receiving the first byte of the response.
        - Waiting is the time between the last byte of the request and the first
          byte of the response
- siege
    - a few more features than `ab`
    - lets you somewhat simulate real users by having a list of URLs to hit in
      sequence
    - https://www.sonassi.com/blog/magento-kb/why-siege-isnt-an-accurate-test-tool-for-magento-performance
    - pretty similar to `ab` in usage
- https://k6.io/
    - https://docs.k6.io/
    - JS scripting, tool itself written in go
- pronk
    - https://github.com/bos/pronk
    - written in Haskell, seems unmaintained
    - similar feature-set to `ab`, `siege`
- httperf
    - https://github.com/httperf/httperf
- tsung
    - http://tsung.erlang-projects.org/
    - can test many protocols: http, postgres, ldap, jabber, mysql, soap, MQTT
      and AMQP
