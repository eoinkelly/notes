# CGI

- https://en.wikipedia.org/wiki/Common_Gateway_Interface
- RFC: https://tools.ietf.org/html/rfc3875

A basic webserver

- knows how to turn a HTTP get for some URL to a particular document within its
  docs dir `/path/to/htdocs`
- has, by convention, a special dir where requests for documents do not yield
  the document itself but the output of executing that "document"
    - the "document" is a binary executable or script
    - sub directory name is usually `/cgi-bin`
- scripts in `cgi-bin/`
    - STDOUT is sent over the wire to the HTTP client
    - Info about the request are stored in the environment that the script
      inherits.
    - In perl this is `%ENV`
- The CGI spec defines the headers the script should get by being invoked by the
  server
- CGI assumes that the web server will fork a new process (duplicate itself,
  edit environment of the dupe and load new code) to run the script
    - This is not super fast as you might imagine

Options for speeding up CGI

1. Web server plugin
    - does not fork new process to run script
    - webserver loads script and interperter into memory at startup (it
      "preforks")
    - examples: `mod_php`
2. FastCGI
    - basically CGI without the requirement that server fork every time
3. Replace webserver
    - Java does this - the java process acts as web server and "application"
      runner
