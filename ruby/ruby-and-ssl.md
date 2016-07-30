The TL;DR of "Does ruby automatically verify SSL certificates" is "yes, almost certainly". This is what I have learned:

#### There are two ways to make secure connections in ruby

1. Use `Net::HTTP`
2. Use the lower level `OpenSSL::SSL::SSLSocket.new` (useful if you are securing something other than HTTP e.g. IMAP)

#### Net:HTTP started verifying certs by default in ruby 1.9.2

* `Net:HTTP` in Ruby 1.9.2 and later does the right thing and sets `OpenSSL::SSL::VERIFY_PEER`.
    * Prior to 1.9.2 it did not do this and that is the source of many of the blog posts from around 2011 that complain about this.
    * It was a PITA to figure out _when_ this actually changed but I was eventually able to work it out by comparing
        ```
        1.9.2: https://github.com/ruby/ruby/blob/ruby_1_9_2/lib/net/http.rb
        1.9.1: https://github.com/ruby/ruby/blob/ruby_1_9_1/lib/net/http.rb
        (search for `VERIFY_MODE` in those files - it turns up the following
        line of doc comment in 1.9.2 file but not older ones)
        ```

> If you set `:use_ssl` as true, you can use https and default value of #verify_mode is set as OpenSSL::SSL::VERIFY_PEER.

#### OpenSSL::SSL::SSLSocket.new still does not verify certs by default

The lower level interface can verify certs but does not do so by default - more
details on this are available in this [Braintree blog
post](https://www.braintreepayments.com/blog/sslsocket-verify_mode-doesnt-verify/)

This is relevant if you write your own code for doing secure connections or
your favourite http-client gem uses the lower level interface and doesn't get
this right

#### Net::HTTP wrappers work in different ways

Net::HTTP isn't the best API so there are many popular gems that provide a better API. These gems have a variety of ways they work

* Some call `Net::HTTP` internally (e.g. httparty)
* Some use the lower level `OpenSSL::SSL::SSLSocket` internally
* Some do something else e.g. typhoeus uses libcurl (which presumably uses its own openssl internally)
