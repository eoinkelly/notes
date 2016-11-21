# %uXXXX encoding

* Non standard - do not use in dev work.
* `%uXXXX` where `XXXX` is the UTF-16 code unit represented as 4 hex digits
* WARNING: not specified by any RFC
* It was considered for the IRI (internationalized version of URI) by W3C but rejected because:
    > Instead of using the existing percent-encoding convention of URIs,
    > which is based on octets, the idea was to create a new encoding
    > convention, for example to use '%u' to introduce UCS code points. Using
    > the existing octet-based percent-encoding mechanism does not need an
    > upgrade of the URI syntax, and does not need corresponding server
    > upgrades.
* Should not be used to encode characters - instead encode them as UTF-8 and prefix each hex pair with %
* it may have security/pen testing uses if software supports it for legacy reasons
