
questions
	 I can ask a server to be recursive, can i ask it/forceit to find an authoritive answer?
	 Am I sure that my machine or local router isn't caching answers when I use nslooup and friends

dig -x IPADDRESS does a reverse lookup

DNS Servers
===========

Telecom http://telecom.custhelp.com/app/answers/detail/a_id/1274
202.27.158.40	shelob.xtra.co.nz
202.27.156.72	ungoliant.xtra.co.nz

opendns.org
208.67.222.222
208.67.220.220

iserve

ns1.iserve.net.nz
202.191.33.129
ns2.iserve.net nz
202.191.32.85


eircom.net
159.134.0.1 dns1.eircom.net
159.134.0.2 dns2.eircom.net

Google DNS
 8.8.8.8
 8.8.4.4

Vodafone NZ/ihug
Preferred DNS server: 203.109.129.67 ns1.ihug.co.nz
Alternate DNS server: 203.109.129.68 ns2.ihug.co.nz


http://nzrs.net.nz/ is a charity that manages the .nz domain

Currently we update the second level zones every hour with the changes made over the last hour and do a full rebuild once a day.

.nz is our first level domain
.co.nz is our second level zone

There are 7 servers that hold the .nz zone and the second level zones (.co.nz etc.)
ns1.dns.net.nz	NZRS	New Zealand	Unicast	202.46.190.130 	2001:dce:2000:2::130
ns2.dns.net.nz	NZRS	New Zealand	Anycast	202.46.191.130  2001:dce:4000:2::130
ns3.dns.net.nz	NZRS	New Zealand	Anycast	202.46.188.130
ns4.dns.net.nz	NZRS	New Zealand	Anycast	202.46.189.130
ns5.dns.net.nz	NeuStar UltraDNS Multiple International	Anycast	204.74.112.253 2001:0502:D399::253
ns6.dns.net.nz	NeuStar UltraDNS Multiple International	Anycast	204.74.113.253
ns7.dns.net.nz	Autonomica	Multiple International	Anycast	194.146.106.54





*	Empty the cache on my local machine
		Windows: ipconfig /flushdns


	Empty the cache on my local router
		?
		just don't use it as the DNS server - tell nslookup to use a specific server


	Ask a particular name server what it thinks the information for a given domain is
		nslookup -type=any [domain] [nameserver]


The New Zealand Shared Registry System (SRS) is a computer system for managing a domain name registry. The SRS was developed in 2002 to manage the .nz domain name space and was released under the GPL Open Source License on 30 January 2004. The SRS is software (client and server) that is used for managing the .nz domain name space and allows the registration, by authorised registrars, of domain names and modification of information associated with that name on the register.

Whois
=====
The host name for the WHOIS server is

     whois.srs.net.nz.
http://nzrs.net.nz/whois/specification
  uses port 43

Get all info about domain foo.co.nz
	whois -h whois.srs.net.nz foo.co.nz

Web based whois at http://dnc.org.nz/

Pointers & ALiases
==================
A Domain Name Pointer is a Domain Name that redirects a one domain name to another domain name, so that your site could have two names, for instance - aiso.net and aiso.org. The domain redirects the user & does not stay in the users browser. So if the domain pointer is aiso.org and the user enters aiso.org into their browser, their browser would get 301 redirected to aiso.net and aiso.org would not be seen in their browser anymore.

A Domain Name Alias is where the domain name that is aliases serves up the files from another domain name. The domain name that is aliased stays in the users browser and the user never sees the domain name that its pulling files from. The domain alias works in this fashion: when you type in the domain name aliases domain xyz.com which points to the main web hosting account domain abc.com, the domain URL in the browser remains xyz.com but the files you see are coming from abc.com/xyz, or if you choose abc.com. The user doesn't see abc.com they see xyz.com within their browser. Or you can have a sub-domain of xzy.abc.com and the files will be pulled from abc.com/xyz but appear in the browser as xyz.abc.com