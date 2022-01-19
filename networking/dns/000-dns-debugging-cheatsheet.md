## Flush local DNS/mDNS caches

1. Flush the libc cache on your local machine (see below)
1. Use `@1.2.3.4` with `dig` to tell it exactly which resolver nameserver to use - this lets you decide whose cache you are going to get an answer from.

```bash
# macOS: flush DNS cache
sudo killall -HUP mDNSResponder

# Ubuntu 18: show stats on DNS cache
sudo systemd-resolve --statistics

# Ubuntu 18: flush DNS cache
sudo systemd-resolve --flush-caches

# Ubuntu 16: flush DNS cache
# Ubuntu 16 does not cache DNS lookups by default
sudo systemctl restart nscd
sudo systemctl restart dnsmasq # optional, only if you are running dnsmasq

# Windows 8-10 flush DNS cache
ipconfig /flushdns
```

How to clear the Chrome DNS cache: Visit chrome://net-internals/#dns


## Dig options

```bash
# dig {options} @{server} {search-string} {type}

# +trace is pretty useful if you need to see the resolvers used
dig -t any +trace @8.8.8.8 foo.bar.com
```

## Misc notes

* The `ANY` query
    * Cloudflare doesn't support ANY query on its authorities servers
    * You can still use it against many resolver servers e.g. 8.8.8.8. They will be giving you just what they have in cache.
* `curl` has a `--dns-servers` option but sadly it seems to not be enabled on macOS and Ubuntu at least


QUESTION: How can I see which DNS responses are cached on your local machine?
