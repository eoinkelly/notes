
* https://support.dnsimple.com/articles/caa-record/
    * says that dig doesn't support them yet but as of 2019-11-04 at least on macOS it does
* a record which says which domains can issue certs for a given domain

https://en.wikipedia.org/wiki/DNS_Certification_Authority_Authorization

> As of June 2018, Qualys reports that still, only 3.4% of the 150,000 most popular TLS-supporting websites use CAA records.

Q: In what way is CAA "mandatory"?

A: It is mandatory for the CAs to support it by checking for a CAA record before they issue a cert

> As of September 8, 2017, all public certificate authorities are required to
> respect CAA records. Before issuing a certificate for a domain, they must
> check the domain for CAA records, and refuse to issue if the CAA record set
> doesn't authorize them. (If there is no CAA record, they are allowed to
> issue.)

