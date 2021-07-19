# mitm proxy

## Install

```bash
# install
brew install mitmproxy

# run
mitmproxy # run proxy and CLI curses interface
# OR
mitmweb # runs proxy and web interface
# OR
mitmdump
```

## mitmproxy CLI commands

* vim commands work for basic navigation, as do arrow keys
    * most functionality is accessed through a vim-alike "command" mode
* `?` to show help for whatever screen you are on
* Export a request+response to a text file:
    1. navigate into the flow you care about
    1. then `:` to go into command mode
    1. then `export.file raw @focus output.file`
    1. to export the request and response as a raw file (text will be plain text, req/response bodies might be binary)

## Setup curl to use proxy

This is a good first step to debug your mitm setup

```bash
# terminal 1
mitmproxy

# terminal 2
curl --proxy 127.0.0.1:8080 --cacert ~/.mitmproxy/mitmproxy-ca-cert.pem https://www.example.com
```

## Setup AWS CLI to use proxy

You do not need to setup your operating system to use the mitmproxy!

```bash
# Step 1: Start mitmproxy in another terminal

# Step 2:
# you need to pass --no-verify-ssl and set the env var to tell the AWS CLI to use your mitm proxy
$ HTTPS_PROXY=127.0.0.1:8080 aws s3 ls --no-verify-ssl
```

## Setup docker to use proxy

https://docs.mitmproxy.org/stable/concepts-certificates/
https://dev.to/natterstefan/docker-tip-how-to-get-host-s-ip-address-inside-a-docker-container-5anh

Run this on the host:

```bash
host$ brew install mitmproxy

# output the .crt to stdout  and capture in clipboard
host$ openssl x509 -in "~/.mitmproxy/mitmproxy-ca-cert.pem" -inform PEM  | pbcopy

container$ vim /usr/share/ca-certificates/mitm.crt
# paste the clipboard and save

container$ dpkg-reconfigure ca-certificates
# you must choose the new cert here - it is usally the first in the list
# this creates file in /etc/ssl/certs which are required to have the cert be active

# Might be required? TODO: check
# container$ update-ca-certificates

# Now the cert should validate
# Note we don't have to use --insecure flag to curl
# host.docker.internal works on macOS/Windows to be the IP address of the host
container$ curl -v https://api.thing.com --proxy host.docker.internal:8080
```

**NOTE: This container will only accept MITM as a CA now so remember to throw it away when you are done**

## Setup web browser to use proxy

Most of the time your browser dev tools should be able to show you what is going on so only do this if you need it.

You might also need to do this if the CLI tool you are debugging ignores system proxy settings

To use mitmproxy with your web browser you need to setup your OS to use it.

### macOS:

1. Open `System Preferences > Network > {Your network} > Advanced ... > Proxies tab`
1. Copy the .cer copy of the MITM CA certificate to somewhere accessible
1. Open the .cer file with default app via `open ~/.mitmproxy/mitmproxy-ca-cert.cer` and add it to your **login** keychain
    * it errors when you try to add it to the default "Local Items" keychain
1. Open _Keychain Access_, choose your _login_ keychain, choose _All items_ and search for `mitm`
1. Open the mitm cert an mark it as trusted.
1. Set both HTTP Proxy and HTTPS proxy to `127.0.0.1`, post `8080` (note no `http://` or `https://` prefix). You must hit save and also `Apply` for the setting to take hold
    * Don't use `localhost` either because ??? (I think it didn't work but can't remember now)


## Appendix: HTTP_PROXY and HTTPS_PROXY env vars

is the http:// and https:// included? some docs show it but it doesn't work for me?
* they seem to work in upper or lower case
