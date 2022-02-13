# Password authenticated key agreement

* https://palant.info/2018/10/25/should-your-next-web-based-login-form-avoid-sending-passwords-in-clear-text/
    * makes a good case for why I can probably ignore it for work


used for setting up a shared key and not be vuln to a MITM

can be solved with
    triple DH
    certificates (TLS does this)

* https://blog.cryptographyengineering.com/2018/10/19/lets-talk-about-pake/
* https://en.wikipedia.org/wiki/Password-authenticated_key_agreement

SRP = Secure remote password agreement
* from the '90s
* first popular PAKE protocol


https://tobtu.com/blog/2021/10/srp-is-now-deprecated/

old, should almost certainly be replaced with pake these days

there are questions around browsers being able to do good randomness