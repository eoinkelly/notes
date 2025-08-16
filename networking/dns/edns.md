# EDNS

- An extension to DNS was desired but the original protocol didn't allow for new
  flags and sections so an _OPT Record_ was added
- https://datatracker.ietf.org/doc/html/rfc6891
- is a "hop by hop" extension i.e. it is negotiated separately between each pair
  of servers
- does N things:
    1. extends the max size of a DNS packet from 512 bytes over UDP (TCP has
       always supported the larger sizes and has been the fallback if the
       message was truncated in UDP)
    2. Defines an OPT record type which can be used to send arbitrary data in a
       request/response
    3. others ???
- EDNS is required for DNSSEC

OPT record type

- https://dns.lookup.dog/record-types/OPT
- is a pseudo record-type
- is often missing
- there should be only one OPT record in the request and/or response
- max size of a DNS message over UDP is 512 bytes - this creates problem for
  future e.g. as IPv6 addresses added

    The OPT RR has RR type 41.

    If an OPT record is present in a received request, compliant responders MUST
    include an OPT record in their respective responses.

    An OPT record does not carry any DNS data. It is used only to contain
    control information pertaining to the question-and-answer sequence of a
    specific transaction. OPT RRs MUST NOT be cached, forwarded, or stored in or
    loaded from master files.

    The OPT RR MAY be placed anywhere within the additional data section. When
    an OPT RR is included within any DNS message, it MUST be the only OPT RR in
    that message. If a query message with more than one OPT RR is received, a
    FORMERR (RCODE=1) MUST be returned. The placement flexibility for the OPT RR
    does not override the need for the TSIG or SIG(0) RRs to be the last in the
    additional section whenever they are present.

- opt record has a fixed and variable part
    - fixed
        - lets requestor tell server what it's UDP payload size will be
        - other stuff too
    - variable _ 0-many options of the form 1. option-code: (assigned by DNSEXT
      working group) _
      http://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml#dns-parameters-11 2.
      option-length (size in bytes of the option-data) 3. option-data: (variable
      length bit field)
