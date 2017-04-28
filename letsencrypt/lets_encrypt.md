# Lets encrypt

# Background

An SSL cert is generally two files

* something.crt (public key)
* something.key (private key)

Your server will give the .crt file (public key) to browsers who will encrypt their requests and the server can then decrypt them with the .key file (private key)

# How-to
Goal: get a certificate file from a CA

Prerequisite: you have to demonstrate you own the domain

How: you need to use the "ACME" protocol to prove you own the domain. Certbot is an ACME client 

* If you have shell access
    * option: use certbot
* If no shell access
    * option: use a hosting provider which provides support via their control panel
        * examples: 
            * wordpress.com enable it by default and automatically redirect
    * option: use certbot in "manual mode" to get the cert on your local machine first and the upload it to hosting provider (provided they support uploading certs)

## heroku

Manual SSL upload instructions: https://devcenter.heroku.com/articles/ssl

Heroku "Automated certificate management" uses Letsencrypt - https://devcenter.heroku.com/articles/automated-certificate-management

Caveats

* can't use ACM if app is behind a CDN like cloudflare or fastly
* can't use wildcard certs
* can't have more than 100 custom domains

I'm not sure whether these are imposed by letsencrypt certs or heroku

## ACME clients

* there are a bunch of clients for cmdline use
* also a bunch of libraries in various languages
* also softwrae like nginx, haproxy has plugins which will automatically verify and renew letsencrypt certs

## certbot (the recommended cmdline ACME client)

https://certbot.eff.org/

```
mkdir -p ~/.letsencrypt/config ~/.letsencrypt/log ~/.letsencrypt/work-dir
alias certbot="certbot --config-dir $HOME/.letsencrypt/config --logs-dir $HOME/.letsencrypt/log --work-dir $HOME/.letsencrypt/work-dir"
```

By default `certbot` uses /etc/letsencrypt/, /var/log/letsencrypt, and /var/lib/letsencrypt which means you need root so we alias to avoid that


* ACME client
* has "manual mode"
    * you manually upload a file to your domain to prove you own it