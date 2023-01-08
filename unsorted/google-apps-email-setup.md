Google Apps Email Setup
=======================

ALT1.ASPMX.L.GOOGLE.COM.    MX    20
ALT2.ASPMX.L.GOOGLE.COM.    MX    20
ASPMX.L.GOOGLE.COM.        MX    10
ASPMX2.GOOGLEMAIL.COM.        MX    30
ASPMX3.GOOGLEMAIL.COM.         MX    30

mail    CNAME    ghs.google.com.

mydomain.co.nz    TXT    v=spf1 include:_spf.google.com ~all


SPF Stuff: http://support.google.com/a/bin/answer.py?hl=en&answer=178723

Mail Subdomain
--------------------
You have to set it up in DNS and in apps admin
apps admin steps: http://support.google.com/a/bin/answer.py?hl=en&answer=53340
Pointing a subdomain to a google service: http://support.google.com/a/bin/answer.py?hl=en&answer=47283
