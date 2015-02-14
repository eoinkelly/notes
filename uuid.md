# UUID

* Universally unique identifiers
* also known as _Globally unique identifier (GUID)_
* not _guaranteed_ to be unique, the probability of a duplicate is extremely low.
* for distributed systems, these identifiers provide a better uniqueness
  guarantee than sequence generators, which are only unique within a single
  database.
* Postgres has a UUID type
* things that provide uuid algorithms
    * on some `*BDS` systems `libc` provides UUID functions
    * on linux the `libuuid` lib provides UUID functions
    * ruby `SecureRandom` and `uuid` gem
    * postgres `owssp` extension
* The relevant standards
    * ITU-T Rec. X.667
    * ISO/IEC 9834-8:2005
    * RFC 4122

    specify four algorithms for generating UUIDs, identified by the version
    numbers 1, 3, 4, and 5. There is no version 2 algorithm. No single
    algorithm is suitable for all use-cases.

# Format

* usually stored in lowercase hex with dashes (the "standard form")
* but may appear without dashes or in upper/mixed case
* 32 hex digits (128 bits)

```
# example:
a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11

# has the format:
{8}-{4}-{4}-{4}-{12}
```

Validation regexp

```
/[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89aAbB][a-f0-9]{3}-[a-f0-9]{12}/
```


# Algorithms

* v1
    * involved mac address of computer and timestamp
    * reveals the identity of the computer that made it
    * UUID is 128 bit long
    * Example of how the Ruby UUID gem creates
        * 60-bit time value (15 hex digits)
            * taken from system clock
        * 16-bit sequence number (4 hex digits)
            * required as clock could be set backwards so time alone does not guarantee uniqueness
            * incremented each time the UUID generator is started
        * 48-bit node identifier (12 hex digits)
            * mac address of the machine used
            * postgres `uuid_generate_v1()` does indeed show my mac address as the final 12 digits
        * postgres `uuid_generate_v1()` does not seem to use this exactly - it generates:
        ```
        # notice only the first 8 hex digits change
        # the last block is the mac address of the en0 interface
        4fe43fc6-b151-11e4-b432-8438354d9c88
        789a1b84-b151-11e4-b432-8438354d9c88
        7ccdeec4-b151-11e4-b432-8438354d9c88
        ```
* v3
    * no random or environment dependant inputs so is fully reproducable
    * `uuid_generate_v3({namespace constant}, {string})`
    * the `{string}` is hashed with MD5 so theory not recoverable (see v5 algorithm for improvement)
* v4
    * derived entirely from random numbers
    * ruby `SecureRandom.uuid` generates v4 UUIDs
* v5
    * same as v3 except the `{string}` is hashed with SHA-1 so more secure
    * this is preferred to v3 now


UUID values cannot be sorted in order of creation

## Implementations

### Postgres

```sql
# postgres
CREATE EXTENSION "uuid-ossp";
SELECT uuid_generate_v4();
# has all algorithms available - see
# http://www.postgresql.org/docs/9.4/static/uuid-ossp.html
```

### Ruby

```ruby
require 'securerandom'
# uses algorithm v4 so is random
# uuid gem: https://github.com/assaf/uuid
SecureRandom.uuid
```

### Sources

* http://www.postgresql.org/docs/9.4/static/uuid-ossp.html
* Using in Rails: http://blog.arkency.com/2014/10/how-to-start-using-uuid-in-activerecord-with-postgresql/
* RFC 4122: http://tools.ietf.org/html/rfc4122
