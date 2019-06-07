# Tools

* https://toolbox.googleapps.com/apps/checkmx/

# SMTP Refesher

Sources

* RFC https://tools.ietf.org/html/rfc5321

SMTP has two kinds of headers

1. Envelope headers (used by mail treansport software to route and deliver email)
2. Message headers (ignored by mail transport software, used by mail client to present UI to the user)

The things which speak SMTP **never** look at the message headers at all. As
far as SMTP is concerned, the email message (headers and all) is just one big
blob that it shuttles around.

Many SMTP clients are perfectly happy to deliver email with badly broken or
entirely nonexistent message headers.

SMTP server response codes are similar but not the same as HTTP server response codes

* 2xx = success
* 4xx = temporary failure, may try again
* 5xx = permenant failure

```bash
# Example SMTP conversation

# variables indicated by $VARIABLE
$ nc $MAIL_SERVER 25
server> $FULLY_QUALIFIED_MAIL_SERVER_HOSTNAME ESMTP Postfix (Ubuntu)
client> HELO $FULLY_QUALIFIED_CLIENT_HOSTNAME
server> 250 $FULLY_QUALIFIED_MAIL_SERVER_HOSTNAME
client> MAIL FROM: $FULLY_QUALIFIED_SENDER_ADDRESS
server> 250 Ok
client> RCPT TO: $FIRST_FULLY_QUALIFIED_RECIPIENT_ADDRESS
server> 250 Ok
client> RCPT TO: $SECOND_FULLY_QUALIFIED_RECIPIENT_ADDRESS
server> 250 Ok
client> DATA
server> 354 Start mail input; end with <CRLF>.<CRLF>
client> $ACTUAL_MAIL_MESSAGE_INCLUDING_MESSAGE_HEADERS
client>.
server> 250 OK, accepted for delivery
client> QUIT
server> 221 Bye
```

Envelope headers:

* `MAIL FROM` (the envelope sender)
* `RCPT TO` (an envelope reciever, may be one of many)

# Sender Policy Framework SPF

* Purpose is to detect forged sender addresses in email
* Dates from 2004 (ish)
* https://tools.ietf.org/html/rfc7208 (main RFC)
    * added to by https://www.rfc-editor.org/rfc/rfc7372.txt
* It is a "proposed standard" in the RFC parlance
* SPF allows the owner of an Internet domain to specify which computers are authorized to send mail with _envelope-from_ addresses in that domain, using Domain Name System (DNS) records
* SPF info must be stored in TXT records
    * An earlier version of the RFC created a new DNS Record type (SPF, type 99) and recommended that but that is now obsolete and should not be used.
* An SPF record is a parsable config language which allows you to specify which computers are allowed to send email for this domain


Recommendation

* Every domain name or host with an A record **or** a MX record should have an SPF record
    * specify the policy if that domain/host is used in an email address or as HELO/EHLO argument
    * if this host does not send mail it should have a `"v=spf1 -all"` SPF policy which fails all


	https://support.google.com/a/answer/33786?hl=en
	"v=spf1 include:_spf.google.com ~all"
	NOTE: google recommends `~all` not `-all`


Sources

* https://support.dnsimple.com/articles/spf-record/
* http://www.openspf.org/Project_Overview

```bash
"v=spf1 {SET_OF_RULES_FOR_WHICH_COMPUTERS_ARE_ALLOWED_TO_SEND_EMAIL_FOR_THIS_DOMAIN}"
"v=spf1 {RULE} {RULE}" # rules are separated by whitespace
"v=spf1 ip4:192.0.2.0/24 ip4:198.51.100.123 a -all"

# v={SPF_VERSION}


"v=spf1 mx include:\_spf.google.com -all"
# mx => allow all servers in MX records for this domain
# include:\_spf.google.com => include google mail servers as authorized servers
# -all => fail any other server

> An ~all tag indicates a soft SPF fail while an -all tag indicates a hard SPF
> fail. In the eyes of the major mailbox providers ~all and -all will both result
> in SPF failure. Return Path recommends an -all as it is the most secure record
```

* SPF records cannot be over 255 characters in length and cannot include more than ten include statements, also known as "lookups."

Rule anatomy

1. qualifier
    * possible values
        * `+` for pass
        * `-` for fail
        * `~` for soft fail
        * `?` for neutral
    * `+` is assumed if no qualifier is present


# SenderID

* An attempt to join SPF with Microsoft's CallerID
* Tries to improve on SPF
* Policies are TXT records like spf excpt they begin with `spf2.0/{SOMETHING}`

SenderID in 2019

* Google doesn't have an `spf2.0/` record
	```
	"v=spf1 include:_netblocks.google.com include:_netblocks2.google.com include:_netblocks3.google.com ~all"
	```
* Amazon do have it
	```
	"spf2.0/pra include:spf1.amazon.com include:spf2.amazon.com include:amazonses.com -all"
	"v=spf1 include:spf1.amazon.com include:spf2.amazon.com include:amazonses.com -all"
	```
* Microsoft do not have it
	```
	"v=spf1 include:_spf-a.microsoft.com include:_spf-b.microsoft.com include:_spf-c.microsoft.com include:_spf-ssg-a.microsoft.com include:spf-a.hotmail.com ip4:147.243.128.24 ip4:147.243.128.26 ip4:147.243.1.153 ip4:147.243.1.47 ip4:147.243.1.48 -all"
	```

Conclusion: Can safely ignore Sender ID

# DKIM

Sources

* https://en.wikipedia.org/wiki/DomainKeys_Identified_Mail

Overview

* designed to prevent spoofing of email (spam & phishing)
* RFC https://tools.ietf.org/html/rfc6376
* Standard since 2013

How it works

1. Publish your public key in a DNS record
1. The sending SMTP server adds a digital signature (using private key) to each outgoing email message
1. Receiving SMTP server can verify the signature based on the public key from DNS. This tells them
	1. The message is authorized to come from this domain
	1. The signed part of the message has not been tampered with

When do I need to add a DKIM to my domain's DNS?

https://support.dnsimple.com/articles/dkim-record/

The key will either be inserted directly into your zone as a TXT record, or it will be a CNAME pointing to the key in your provider’s DNS
