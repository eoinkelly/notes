Async HTTP requests in ruby

Options

- raw threads
- fibers
    - http://ruby-doc.org/core-2.2.2/Fiber.html
- event machine
    - http://rubyeventmachine.com/
    - https://github.com/igrigorik/em-http-request/wiki/Issuing-Requests
- Typheous
    - based on "Ethon" (a simple libcurl wrapper)
        - https://github.com/typhoeus/ethon
    - a wrapper around libcurl
    - https://github.com/typhoeus/typhoeus
- Faraday
    - provides a common interface to many http-client libs
    - wraps
        - Net::HTTP
        - Typheous
    - example https://gist.github.com/ezkl/2272636
