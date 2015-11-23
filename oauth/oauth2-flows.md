
# Experiment: get access to a protected resource on a doorkeeper rails app using just curl

Following
https://github.com/doorkeeper-gem/doorkeeper/wiki/authorization-flow

## Step 1: register my "client" with the "authorization server" (doorkeeper)

* Visit http://localhost:3000/oauth/applications and use the UI there to create an application
    * Use the special `urn:ietf:wg:oauth:2.0:oob` as the client redirect URL for testing
        * this causes doorkeeper to show the URL rather than redirect to it
        * this can be configured to be any string (or this no-redirect can be disabled)
        * the given string seems at least conventional if not standard
    * Doorkeeper calls the "client redirect URL" a "Callback URL"
    * In doorkeeper and application contains
        * application ID (called `client identifier` in the spec)
        * application secret (called `client secret` in the spec)
        * scopes (space sepearated string)
        * callback URLs (note: plural)
            * how do multiple callback URLs work? are they hit round-robin? all each time?

## Step 2: Authorise the client application with doorkeeper

Click on the doorkeeper UI 'Authorize' button which sends:

```
# URL line broken for readability
http://localhost:3000/oauth/authorize?
    client_id=4103f22830d27220bbce6d5063ca3dcdf3d3470f85888fadd6c3511c2118303f
    &redirect_uri=urn%3Aietf%3Awg%3Aoauth%3A2.0%3Aoob
    &response_type=code
```

* response_type=code means that we are requesting to use the "authorization code" grant type

This requests an authorization code. The user is asked to authenticate if they
have not already done so and then you are shown a page with an "authorization
code"

## Step 3: Request an access token

Take the authorization code from the previous step and use it to get an access token

```bash
#!/bin/bash

# ./convert_grant_to_access_token.sh

client_id='4103f22830d27220bbce6d5063ca3dcdf3d3470f85888fadd6c3511c2118303f'
client_secret='8a216cc7a12693d4ea3137eeac54d0a72e89b0006111d670308f415daae2f64f'
auth_code='c9e0a9216b237345228f3545df2f5ec48df4facd838879db50ba37df79e33394'

http -v post http://localhost:3000/oauth/token \
  client_id=$client_id \
  client_secret=$client_secret \
  code=$auth_code \
  grant_type=authorization_code \
  redirect_uri=urn:ietf:wg:oauth:2.0:oob
```

```
$ ./convert_grant_to_access_token.sh
POST /oauth/token HTTP/1.1
Accept: application/json
Accept-Encoding: gzip, deflate
Connection: keep-alive
Content-Length: 323
Content-Type: application/json
Host: localhost:3000
User-Agent: HTTPie/0.9.2

{
    "client_id": "4103f22830d27220bbce6d5063ca3dcdf3d3470f85888fadd6c3511c2118303f",
    "client_secret": "8a216cc7a12693d4ea3137eeac54d0a72e89b0006111d670308f415daae2f64f",
    "code": "c9e0a9216b237345228f3545df2f5ec48df4facd838879db50ba37df79e33394",
    "grant_type": "authorization_code",
    "redirect_uri": "urn:ietf:wg:oauth:2.0:oob"
}

HTTP/1.1 200 OK
Cache-Control: no-store
Connection: Keep-Alive
Content-Length: 147
Content-Type: application/json; charset=utf-8
Date: Fri, 13 Nov 2015 01:40:55 GMT
Etag: W/"d12fdcafcdfbffe078c7e79d007f805a"
Pragma: no-cache
Server: WEBrick/1.3.1 (Ruby/2.2.3/2015-08-18)
X-Content-Type-Options: nosniff
X-Frame-Options: SAMEORIGIN
X-Request-Id: 1ade5413-08f8-43d9-9488-a8802c369030
X-Runtime: 0.047205
X-Xss-Protection: 1; mode=block

{
    "access_token": "2dc7fedf1c59eeb78345e4bfaf45ea6552038ebcd9dc94d47b25b39780c20fdd",
    "created_at": 1447378855,
    "expires_in": 7200,
    "token_type": "bearer"
}
```

## Step 4: Get the protected resource

Note that the correct way to encode an access token as the HTTP Basic Auth header is

```
Authorization: Bearer <access token>
Authorization: Bearer 2dc7fedf1c59eeb78345e4bfaf45ea6552038ebcd9dc94d47b25b39780c20fdd
```

Example run:

```
$ http -v http://localhost:3000/posts.json Authorization:"Bearer 2dc7fedf1c59eeb78345e4bfaf45ea6552038ebcd9dc94d47b25b39780c20fdd"
GET /posts.json HTTP/1.1
Accept: */*
Accept-Encoding: gzip, deflate
Authorization: Bearer 2dc7fedf1c59eeb78345e4bfaf45ea6552038ebcd9dc94d47b25b39780c20fdd
Connection: keep-alive
Host: localhost:3000
User-Agent: HTTPie/0.9.2


HTTP/1.1 200 OK
Cache-Control: max-age=0, private, must-revalidate
Connection: Keep-Alive
Content-Length: 244
Content-Type: application/json; charset=utf-8
Date: Fri, 13 Nov 2015 01:52:29 GMT
Etag: W/"687ee266bce9496d0574063ab057663f"
Server: WEBrick/1.3.1 (Ruby/2.2.3/2015-08-18)
X-Content-Type-Options: nosniff
X-Frame-Options: SAMEORIGIN
X-Request-Id: 54483694-29c4-4e15-8133-7a826b201e89
X-Runtime: 0.022549
X-Xss-Protection: 1; mode=block

[
    {
        "body": "I am secret",
        "id": 1,
        "published": false,
        "title": "Super secret post",
        "url": "http://localhost:3000/posts/1.json"
    },
    {
        "body": "very hush hush",
        "id": 2,
        "published": false,
        "title": "Another secret post",
        "url": "http://localhost:3000/posts/2.json"
    }
]
```

## step 3 with refresh tokens enabled in doorkeeper

note that the reply has an extra `refresh_token` field

```
OST /oauth/token HTTP/1.1
Accept: application/json
Accept-Encoding: gzip, deflate
Connection: keep-alive
Content-Length: 323
Content-Type: application/json
Host: localhost:3000
User-Agent: HTTPie/0.9.2

{
    "client_id": "4103f22830d27220bbce6d5063ca3dcdf3d3470f85888fadd6c3511c2118303f",
    "client_secret": "8a216cc7a12693d4ea3137eeac54d0a72e89b0006111d670308f415daae2f64f",
    "code": "275756bfc798a708eca42fa5b72a13739d076c9733a9273cb138d6220a647c0e",
    "grant_type": "authorization_code",
    "redirect_uri": "urn:ietf:wg:oauth:2.0:oob"
}

HTTP/1.1 200 OK
Cache-Control: no-store
Connection: Keep-Alive
Content-Length: 230
Content-Type: application/json; charset=utf-8
Date: Fri, 13 Nov 2015 01:58:11 GMT
Etag: W/"8064b4eb286348131985d4e129f090d6"
Pragma: no-cache
Server: WEBrick/1.3.1 (Ruby/2.2.3/2015-08-18)
X-Content-Type-Options: nosniff
X-Frame-Options: SAMEORIGIN
X-Request-Id: ee3516dd-de0f-42f4-97af-eec6fa74a199
X-Runtime: 0.035231
X-Xss-Protection: 1; mode=block

{
    "access_token": "db3be06e9652c42463612ba51ad3fcdaed49e4a7792f818c7a45e68efb1d4db3",
    "created_at": 1447379891,
    "expires_in": 7200,
    "refresh_token": "811e135d0ae9ca69888b42fe3e2d4c605a5dc63a5b9cf116a63679a93b7e7345",
    "token_type": "bearer"
}
```

# Getting a new access token using a refresh_token

* the token refresh is POSTed to the same endpoint as the authorization grant was
* refreshing a token is just using slightly different credentials (a refresh token instead of an authorization code) to get an access token
* notice that we get back a new access token AND a new refresh token

```bash
#!/bin/bash

# ./refresh_token.sh

client_id='4103f22830d27220bbce6d5063ca3dcdf3d3470f85888fadd6c3511c2118303f'
client_secret='8a216cc7a12693d4ea3137eeac54d0a72e89b0006111d670308f415daae2f64f'
refresh_token='811e135d0ae9ca69888b42fe3e2d4c605a5dc63a5b9cf116a63679a93b7e7345'

http -v post http://localhost:3000/oauth/token \
  client_id=$client_id \
  client_secret=$client_secret \
  refresh_token=$refresh_token \
  grant_type=refresh_token \
  redirect_uri=urn:ietf:wg:oauth:2.0:oob
```

```
$ ./refresh_token.sh
POST /oauth/token HTTP/1.1
Accept: application/json
Accept-Encoding: gzip, deflate
Connection: keep-alive
Content-Length: 339
Content-Type: application/json
Host: localhost:3000
User-Agent: HTTPie/0.9.2

{
    "client_id": "4103f22830d27220bbce6d5063ca3dcdf3d3470f85888fadd6c3511c2118303f",
    "client_secret": "8a216cc7a12693d4ea3137eeac54d0a72e89b0006111d670308f415daae2f64f",
    "grant_type": "refresh_token",
    "redirect_uri": "urn:ietf:wg:oauth:2.0:oob",
    "refresh_token": "811e135d0ae9ca69888b42fe3e2d4c605a5dc63a5b9cf116a63679a93b7e7345"
}

HTTP/1.1 200 OK
Cache-Control: no-store
Connection: Keep-Alive
Content-Length: 230
Content-Type: application/json; charset=utf-8
Date: Fri, 13 Nov 2015 02:22:19 GMT
Etag: W/"9f3cea3f46b4481d043672c45a11215f"
Pragma: no-cache
Server: WEBrick/1.3.1 (Ruby/2.2.3/2015-08-18)
X-Content-Type-Options: nosniff
X-Frame-Options: SAMEORIGIN
X-Request-Id: 1b3c7925-7c07-4dea-93a1-1441569fc266
X-Runtime: 0.076694
X-Xss-Protection: 1; mode=block

{
    "access_token": "3c81fbb129996fa167a7fd3450ddacb1b5d2cfae8cb80fcd907f504ed42e8fd5",
    "created_at": 1447381339,
    "expires_in": 7200,
    "refresh_token": "b3a9227025c62b4b700d10e34c5a04ab6a58949c157bc913ddb25ce59628b371",
    "token_type": "bearer"
}
```


