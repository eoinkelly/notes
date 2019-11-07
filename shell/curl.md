
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
