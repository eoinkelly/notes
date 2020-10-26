# Password hashing

TODO: flesh this out

## Background on password storage

* A _password hash_ is created by feeding the user's real password into an algorithm which generates the hash as output. The "hash" looks like a random string of characters e.g. `$P$B4J4RkvSe3QowfF/v6oHionn8CyW.a.`.
* The first part of the hash `$P$` identifies which algorithm was used.
* hash functions are deterministic (they will always generate the same output for a given input)
    * -- an attacker can inspect the hashes in the DB and see which user's passwords are the same
        * salts mitigate this issue

## Salt

* a unique string (different for each user) which is stored in the DB with the user and the password hash

* benefits
    1. makes passwords more complex
Goals

* it stops the same user password from hashing to the same thing each time
* using salts means that password duplication doesn't appear as hash duplication in the database
* using salts mean that an attacker has to create a rainbow table for each user hash (slows them down)
    * salts are speed bumps to cracking the passwords - they don't prevent it!
* salts are stored in cleartext in the users table
* salts should be cryptographically random
* salts can be prepended or appended to the password

What does devise do?

## Pepper

* pepper is a random string but it is not stored in the DB
    * can be stored in env of server or on disk - ideally somewhere iwth a security boundary between it and the DB so if an attacker pops one they can't get the other
* if an attacker gets your db (hashes and salts) but not the pepper then cracking the password is that much harder for them
* Where the salt only has to be long enough to be unique, a pepper has to be secure to remain secret (at least 112 bits is recommended by NIST), otherwise an attacker only needs one known entry to crack the pepper.
* even if the pepper is breached, an attacker still has to brute force the hashes

Goal

* provide some protection against somebody popping the DB and stealing the users table
    * it makes it harder for them to get from the password hashes to the original password

## Bcrypt

hashes and salts?
a bcrypt hash usually begins with the marker $2b$

## References

* Good overview of salting https://auth0.com/blog/adding-salt-to-hashing-a-better-way-to-store-passwords/

