# Spock

can take params from
    HTTP_CLIENT_SECRET
    HTTP_CLIENT_ID
or from client_secret and client_id params

# With doorkeeper

* OAuth spec requires application/x-www-form-urlencoded for POST requests but
  Doorkeeper will also accept `multipart/form-data` (presumably because rails
  decodes it into `params` for doorkeeper and rails does that)

```
# making a client-credentials token request using -form-data encoding
curl
-X POST
-H "Cache-Control: no-cache"
-H "Postman-Token: d41a1431-d5dc-1eb4-5e79-05e38c6ba40f"
-H "Content-Type: multipart/form-data; boundary=----WebKitFormBoundary7MA4YWxkTrZu0gW"
-F "grant_type=client_credentials"
-F "client_id=eba574524069f02b9102a13ff609a2d41ec49647456f11da59b6e7f06186d4ed"
-F "client_secret=7dfa6914499eacb75defe64ea4619801da125a02abfa11ac8ef7f3da08c07a59"
"http://localhost:3001/oauth/token/"

# making the same request with x-www-form-urlencoded
curl
-X POST
-H "Cache-Control: no-cache"
-H "Postman-Token: b6631134-24f2-95e3-a969-903898c42481"
-H "Content-Type: application/x-www-form-urlencoded"
-d 'grant_type=client_credentials&client_id=eba574524069f02b9102a13ff609a2d41ec49647456f11da59b6e7f06186d4ed&client_secret=7dfa6914499eacb75defe64ea4619801da125a02abfa11ac8ef7f3da08c07a59'
"http://localhost:3001/oauth/token/"

# with either method the response is:
{
  "access_token": "79aac433cb22be0ee7e02f94cd337b7d3bd672db3f1041d188e5249320f4735b",
  "token_type": "bearer",
  "expires_in": 7200,
  "created_at": 1470453409
}
```

How doorkeeper allows the client-application to authenticate itself

1. it first checks HTTP_AUTHORIZATION header
    * QUESTION: waht is format of that header
1. then checks `:client_id` and `:client_secret` params from the rails `params` object


