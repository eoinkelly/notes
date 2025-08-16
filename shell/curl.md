```bash
# To see connection metadata but not the response body:
#
curl -svL www.foo.com 1>/dev/null
#
# Explanation:
#
# * protocol defaults to http:// if unspecified
# * -s = silent (suppresses progress bars in output)
# * -v = verbose (shows TLS setup and HTTP headers)
# * -L = location (follows redirects)
# * 1>/dev/null = discard the response body (-v writes stuff to stderr so that will still be shown)


# Host trick

# You can manually insert values into curls DNS cache
curl --resolve example.com:12.34.12.34 www.example.com


# HTTP Basic auth
curl --user user:pass www.example.com

# same but more explicity
curl --basic --user user:pass www.example.com

# digest auth
curl --digest --user user:pass www.example.com

curl http://example.com

# --http2 = use HTTP2
# -4 = force IPv4
# -6 = force IPv6
# --data -d --data-ascii = send data in a POST request
# --data-raw = send data but don't do special interpretation of @ character
# --data-urlencode
# --data-binary = post data with no extra processing (you can use use @file.txt)
#
# --form -f = upload as if a web form was used
#
# -H "Host:" # remove an existing Host: header
# -H "X-Some-Header: Value" # add a header
#
# --head -I make HEAD request only
# --output -o <file> = write output to file instead of stdout
# -O = use the name of the file in the URL as the local name of the output file

# Dumping info
#
# --include -i = show HTTP headers in output (less verbose than -v)
# --verbose -v = verbose mode (this is the most useful trace command I think)
#     * lines begin with > are headers sent
#     * lines begin with < means headers received
# --trace <file> = show a full hex trace of all incoming and outgoing data in output
# --trace-binary <file> = show a full hex trace of all incoming and outgoing data in output

curl http://example.com --trace - # trace to stdout


curl http://example.com -d name=foo -d age=12
# becomes name=foo&age=12
cat stuff.txt | curl http://example.com -d - # read data from stdin

curl http://example.com -d @stuff.txt
# read data from file

curl http://example.com --form profile=@portrait.jpg
```

## Hitting a local nginx

Scenario:

You have an nginx server which terminates TLS and proxies requests to an
application (e.g. Rails, Django PHP-fpm etc.)

The app will (sensibly) re-direct any HTTP requests to their HTTPS equivalent so
you must make a HTTPS request to get a valid response.

The nginx server cert is signed but your `curl` installation probably doesn't
have access to the certs required to verify it

How do you test that your application works from an SSH session in the VM
running nginx and the app?

Challenges

1. You have to provide a `Host` HTTP header to tell nginx which site you want
   responses from
    - Easy: pass the `-H "Host: myhostname.example.com"` option to curl
2. You have to tell curl that it should not verify the cert because it probably
   doesn't have access to the correct root certificates
    - Easy: pass the `--insecure` option to curl
3. You must fool SNI
    - Harder

```bash
# for --resolve the host:port:address -  the address must be a numeric ip

# the domain names must match exactly in --resolve and what you use in your url
curl --resolve myapp.myorg.org.au:127.0.0.1 -H "Host: myapp.myorg.org.au" https://127.0.0.1/users/sign_in -v --insecure
```

       --resolve <host:port:address>
              Provide  a custom address for a specific host and port pair. Using this, you can make the curl requests(s) use a specified address and prevent the otherwise nor‐
              mally resolved address to be used. Consider it a sort of /etc/hosts alternative provided on the command line. The port number should be the number used  for  the
              specific protocol the host will be used for. It means you need several entries if you want to provide address for the same host but different ports.

              This option can be used many times to add many host names to resolve.

              (Added in 7.21.3)

       --connect-to <HOST1:PORT1:HOST2:PORT2>

              For a request to the given HOST1:PORT1 pair, connect to HOST2:PORT2 instead.  This option is suitable to direct requests at a specific server, e.g.
              at a specific cluster node in a cluster of servers. This option is only used to establish the network connection. It does NOT affect the
              hostname/port that is used for TLS/SSL (e.g. SNI, certificate verification) or for the application protocols. "HOST1" and "PORT1" may be the empty
              string, meaning "any host/port". "HOST2" and "PORT2" may also be the empty string, meaning "use the request's original host/port".

              A "host" specified to this option is compared as a string, so it needs to match the name used in request URL. It can be either numerical such as
              "127.0.0.1" or the full host name such as "example.org".

              This option can be used many times to add many connect rules.

              Example:
               curl --connect-to example.com:443:example.net:8443 https://example.com

              See also --resolve and -H, --header. Added in 7.49.0.
