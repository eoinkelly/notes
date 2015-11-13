
# Experiment: get access to a protected resource on a doorkeeper rails app using just curl

Step 1: register my "client" with the "authorization server" (doorkeeper)

* Visit http://localhost:3000/oauth/applications and use the UI there to create an application
    * Use the special `urn:ietf:wg:oauth:2.0:oob` as the client redirect URL for testing
        * this causes doorkeeper to show the URL rather than redirect to it (I think)
    * Doorkeeper calls the "client redirect URL" a "Callback URL"
    * In doorkeeper and application contains
        * application ID (called `client identifier` in the spec)
        * application secret (called `client secret` in the spec)
        * scopes (space sepearated string)
        * callback URLs (note: plural)
            * how do multiple callback URLs work? are they hit round-robin? all each time?

# Step 2: Authorise the client application with doorkeeper

```
# URL line broken for readability
http://localhost:3000/oauth/authorize?
    client_id=4103f22830d27220bbce6d5063ca3dcdf3d3470f85888fadd6c3511c2118303f
    &redirect_uri=urn%3Aietf%3Awg%3Aoauth%3A2.0%3Aoob
    &response_type=code
```

* response_type=code means that we are requesting to use the "authorization code" grant type

```
GET /oauth/authorize?client_id=4103f22830d27220bbce6d5063ca3dcdf3d3470f85888fadd6c3511c2118303f&redirect_uri=urn%3Aietf%3Awg%3Aoauth%3A2.0%3Aoob&response_type=code HTTP/1.1
Accept: */*
Accept-Encoding: gzip, deflate
Connection: keep-alive
Host: localhost:3000
User-Agent: HTTPie/0.9.2



HTTP/1.1 302 Found
Cache-Control: no-cache
Connection: Keep-Alive
Content-Length: 101
Content-Type: text/html; charset=utf-8
Date: Thu, 12 Nov 2015 21:33:20 GMT
Location: http://localhost:3000/users/sign_in
Server: WEBrick/1.3.1 (Ruby/2.2.3/2015-08-18)
X-Content-Type-Options: nosniff
X-Frame-Options: SAMEORIGIN
X-Request-Id: aaa1fae2-a05a-4bdf-bb80-b71d4b738f2e
X-Runtime: 0.007108
X-Xss-Protection: 1; mode=block

<html><body>You are being <a href="http://localhost:3000/users/sign_in">redirected</a>.</body></html>
```


# Step 3: Request an access token


# Step 4: Get the protected resource

