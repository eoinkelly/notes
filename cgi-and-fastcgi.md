# CGI

https://en.wikipedia.org/wiki/Common_Gateway_Interface

- Defined in https://tools.ietf.org/html/rfc3875
- is basically _What if we took what we do on the command line and made the web
  server be the user_
- spins up a new process to handle every request
- passes it the HTTP request headers as a bunch of environment variables and the
  contents of the request body (for PUT or POST) are sent to stdin on the
  process
- spins up and tears down a new process to handle each request
    - ++ makes it simple
    - -- not very scalable
- used to interface arbitrary processes with web servers

# FastCGI

https://en.wikipedia.org/wiki/FastCGI

- is a binary protocol **not** an implementation
- spec: http://www.mit.edu/~yandros/doc/specs/fcgi-spec.html
- has bindings for many languages and servers
- supported by all webservers including Apache
- used to interface arbitrary processes with web servers
- created to be a more performant version of CGI
- created by "Open Market" who added it to their web server in mid 1990's as
  response to Netscape server's NSAPI
- appeard around the same time as mod_php and mod_perl were added to apache
  (loading modules into the webserver is a different way to make CGI faster)
- each individual FastCGI process can handle **multiple** requests over its
  lifetime
- FastCGI vs in-process modules have different trade-offs
    - being able to restart PHP/Perl without restarting the server can be good
    - handy for shared hosting when you want to send different requests to
      different FastCGI servers (which have different security policies)

https://www.nginx.com/resources/wiki/start/topics/examples/phpfcgi/
