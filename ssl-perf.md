# TLS

Sources

* https://www.youtube.com/watch?v=0EB7zh_7UE4

# from that talk

* Don't use TLS comopression
    * conneciton is already using HTTP compression (gzip)
    * TLS compression has a significant memory allocation overhead e.g. up to
      1MB per connection on server

Elliptic curve ephermeral Diffe-hellman can be favoured as a cipher suite
because of TLS resumption allows reuse of the session


```
# check the TLS status of a server
openssl s_client -connect foo.herokuapp.com:443 -tls1 -tlsextdebug -status
```

Most servers out of the box don't automatically rotate keys so your "perfect forward secrecy" won't work properly


CDNs are good for TLS performance
TLS has an extra 2 RTTs so you want to minimuse theproblem
The closer you can terminate the TLS connection the better the latency


OCSP Online Certificate Status Program

* browser checks with CA that the server cert is still valid
* You will see it as an empty area on the waterfall graph as the browser goes
  off and does the check
* THere are problems with the OCSP protocol
* Chrome doesn't block on the OCSP check, firefox does a live check

OCSP stapling makes getting the OCSP response the servers job. It "staples" the
OCSP response (which has been signed by the CA) to the response it sends the
client

You can see OCSP stapling in the ooutput of s_client if it has happened

```
OCSP response: no response sent
```

Stapled OSCP increases the size of the certificate. If you cert chain is large,
browser will need more than 2 RTT to setup TLS
