
client = a photo printing web service
resource owner = me
resource owner user-agent = my web browser
protected resource = photos

client registration phase
1. client is registered with the authorization server by one of its devs
    * client now has a `client_id` and `client_secret`
    * authorization server now knows things about the client e.g. who built it, contact email, where its HTTP redirect endpoints are

... time passes ...


1. I wish to allow the client to access my photos so I lookup the URL of the authorization endpoint of my photos service in their docs
1. I craft a HTTP request to correctly requiest an authorization grant from the authorization server e.g.
    ```
    todo
    ```
1. my browser sends GET (or POST) the request to the authorization server's "authorization" endpoint asking for the authorization grant
1. the auth server verifies my identity
    * maybe it puts me through a  HTTP Basic username and password login or check some session cookie for a previous login

... other stuff...

1. the authorization server redirects my browser to the client's registered "redirect URL"
    * this might be some URL on the photo printing website
    * I think the access token is transamitted as part of this redirect e.g.
        * `http://printing.photos.com/authed_stuff/eoinkelly?access_token=abc23` or similar ???
