# Base64 encoding

* so named because it uses a character set with 64 possible values (all of which are printable ASCII chars)
* a way of mapping arbitrary bytes into this specially chosen 64 character set
* 3 input bytes becomes 4 output bytes => data gets 133.33% bigger (plus whatever few padding `=` bytes were added)

How to base64 encode

1. Split input into 3 byte chunks (pad with `=` chars if last chunk is not fully 3 bytes)
2. Split each2-byte chunk into 4 x 6-bit fields
    *6-bit field => 2^6 values => 64 possible values
3. Map each field onto a character from the allowed character set and emit it
