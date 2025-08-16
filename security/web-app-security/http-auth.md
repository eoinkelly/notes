# HTTP Auth

There are 3 available schemes

1. Basic
1. Digest
1. NTLM

# Basic

1. combine username and password with single `:` .e.g `username:password`
2. base64 encode the output of previous step

A client may avoid a login prompt when accessing a basic access authentication
by prepending username:password@ to the hostname in the url e.g.

    https://Aladdin:OpenSesame@www.example.com/index.html

# Digest

RFC 2617 (HTTP Authentication: Basic and Digest Access Authentication).

https://en.wikipedia.org/wiki/Digest_access_authentication
http://api.rubyonrails.org/classes/ActionController/HttpAuthentication/Digest.html
http://www.virtuouscode.com/2013/02/04/the-trouble-with-http-digest-authentication/

> The common network libraries support Basic and token headers but never digest.

> to use Digest, clients and servers need to store the HA1. It’s a shared secret
> computed locally by the client and server that’s never transferred over the
> wire. That was fine back in the day when a hashed, salted password was
> considered “secure” because a computer couldn’t brute force it before the heat
> death of the universe. We’re long past those days. An army of AWS instances
> can be dispatched to build a salted rainbow table in no time. It’s a liability
> to have HA1s and, quite frankly, it scares the crap out of me.

> I really loved Digest auth. It was great a way to send authenticated messages
> over HTTP when the payload didn’t need to be encrypted. Now that we have the
> horsepower to run SSL everywhere and salted hashes can be brute forced, I
> can’t recommend HTTP Digest.

It seems that HTTP Digest provides a way of sending authenticated (but not
encryted) HTTP content

Since servers can now run SSL everywhere this is less useful in general now

TODO: implement digest in a rails app

# NTLM

- Microsoft no longer recommends NTLM in applications - MS recommends Kerberos
  since Win2000
- is basically a legacy windows auth mechanism
- IIS can use NTLM to use windows credentials to allow access to a resource via
  HTTP
- More details <https://www.innovation.ch/personal/ronald/ntlm.html>

> In a Windows network, NT LAN Manager (NTLM) is a suite of Microsoft security
> protocols that provides authentication, integrity, and confidentiality to
> users. NTLM is the successor to the authentication protocol in Microsoft LAN
> Manager (LANMAN), an older Microsoft product.
