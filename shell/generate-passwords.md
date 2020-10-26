

Password entropy $E$:

$E = Log_2(R^L)$

* $R$ = number of characters in the pool
* $L$ = length of the password


* A single byte can have a max of 8 bits of entropy (assuming it takes on a totally random bit pattern)
* If byte is ASCII, then high order bit is 0 => max 7 "bits of entropy"
* If you remove control characters (0-31) then entropy goes down
* alphabetic only? reduces it even more


Imagine a single character password.
Think about the number of possible values it can take on (exclude control chars and assume ascii)
Calculate the entropy above using the formula

How many values are in a single ascii char excluding control chars?

128 chars - 32 control char values - 1 x DEL char value = 95 possible values

=> 6.56 bits of entropy per character (assuming you use all punctuation uppercase, lowercase, numbers)

=> an excellent 16 char password =~ 105 bits of entropy

If you assume password is ascii including upper, lower, numbers then num possible values = 62
=> 1 char password = 5.9 bits entropy
=> 16 char apssword = 95 bits entropy

In theory, adding 1 bit of entropy doubles the number of guesses a brute force attack would have to make (in the worst case)




"X bits of entropy" = "the number of bits whose value are unknown"
it is related to the number of possible values by

$$2^{\textrm{bits of entropy}} = \textrm{number of possible values}$$

What I want

* can choose length
* uses the full set of printable characters a-zA-Z0-9 and selected punctuation

* base64
    * -- doesn't use all available characters

## Generating a good password in the shell

1. get array of all 94 viable ascii chars (exclude space)
2. select indexes at random from the array until the desired length is generated

/dev/urandom is a good source or random but binary data

openssl rand -base64 32
