# WireGuard

* https://www.wireguard.com/
* https://www.wireguard.com/papers/wireguard.pdf
* WireGuard securely encapsulates IP packets over UDP.
* is a linux kernel module written in C
    * they hope to be included in mainline kernel eventually
* the project has userland implementations in rust and go
    * wireshark-go (intended for use on macOS , Windows, `*BSD` - currently not complete)
    * wireshark-rs
        * https://github.com/WireGuard/wireguard-rs
        * The current plan is to target macOS (utun-compatible operating systems) for simplicity, but full cross-platform support is the eventual goal
        * seems less complete than the go version

QUESTION: what is "utun driver" on macOS

Aside: handy DNS leak tester = https://www.dnsleaktest.com/

```
# macOS

brew info wireguard-go
brew info wireguard-tools
```
