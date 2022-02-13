# Password hashing

## Sources

* overview of salting https://auth0.com/blog/adding-salt-to-hashing-a-better-way-to-store-passwords/

## Overview

Password hashing algorithms must:

1. be relatively slow and impossible to speed up to make brute forcing difficult
    * you need to pick algorithm and tune to be as slow as you can tolerate
    * but your algo implementation needs to be as fast as is available, otherwise attackers will have an advantage
    * ideally you can tune it to make it slower if required in future
2. be able to handle very short (low entropy) plain texts

Normal hashing algorithms don't have these properties.

Examples of password hashing algorithms

1. Scrypt
    * newer than bcrypt
    * support
        * libsodium https://libsodium.gitbook.io/doc/password_hashing#:~:text=Argon2,with%20the%20exception%20of%20JavaScript.&text=Libsodium%20supports%20Argon2i%20and%20Argon2id.
2. PBKDF2
    * old, older than bcrypt
3. Bcrypt
    * from 1999
    * used everywhere
    * support
        * ruby: https://github.com/bcrypt-ruby/bcrypt-ruby
        * PHP: built-in support
        * Rust: https://docs.rs/bcrypt/latest/bcrypt/
    * not in libsodium
        * why???
4. Argon2
    * won the https://www.password-hashing.net/ competition in 2015
        * a NIST style algorithm competition setup to choose a new good password hashing algorithm
        * created by Jean-philippe Aumasson

## Recommended password hash tuning params

I'm not sure how reliable this info is but it's probably a good start:

> https://twitter.com/Sc00bzT/status/1372209728097517571
>
> Minimum good settings for auth (<10 kH/s/GPU):
>
> bcrypt: 9 (technically it's like ~8.05)
> Argon2i: m≥74219/(3*t-1)*α, t≥3, p=1
> Argon2{id,d}: m≥74219/(3*t-1)*α, t≥1, p=1
> scrypt: N≥475000/r/p*α, r=8, p≥1
> PBKDF2-SHA512: 120,000
> PBKDF2-SHA256: 350,000
> PBKDF2-SHA1: 720,000

## Attacks on password hashes

1. Brute-force attack
   * try all possible strings
2. Dictionary attack
    * using a list of words, hash each one to try and find a match
    * no pre-computation in a dictionary attack
3. Hash table attack
    * use a pre-computed set of hashes
    * hashes can be stored in a traditional hash table or a rainbow table but the attack is the same in either case

### Hash table

* a pre-computed databases of hashes
* dictionaries and random strings are run through the hash algorithm and the results stored in the DB
* a hash table attack can be a "pre-computed dictionary attack" or (given enough storage) a "pre-computed brute force attack"
* is basically a dictionary attack with pre-computation
* you get a lot more speed in exchange for using storage
* every data breach the attacker can get their hands on can be added to their hash table database. This gives them a better chance of cracking common passwords but also maybe cracking your password if you used the same one in different places
* takes a long time to build a hash-table from scratch but only has to be done once per algorithm+params (e.g. you would need to compute a diff hash for bcrypt with 8,9,10 etc. rounds)
* if the password uses a salt, then
    1. the attacker will have the salt because it was stored with the database
    1. the hash-table must be re-computed for every possible password + the salt i.e. you need to rebuild the table for each password


### Rainbow table

* The paper https://lasec.epfl.ch/pub/lasec/doc/Oech03.pdf
* https://www.youtube.com/watch?v=tlr8VRIhBJA (good video overview)
* https://security.stackexchange.com/questions/92865/what-is-the-difference-between-a-hash-table-and-a-rainbow-table-and-how-are-the

Overview

* a refinement to hash tables to let you store the same data in less space in exchange for needing more compute to create the table
* basically obsolete now as they don't provide value compared to GPU based cracking

#### Creating a rainbow table

Terms

* hash-value
    * The output of a hash function
* Pre-image
    * The data which was the input to the hash function (usually a password)
    * every hash-value has (ideally) exactly one pre-image. If there are more than one pre-image then you have collisions which means your hash function has problems

Inputs to rainbow table

1. A hash function H
    * the hash function you want to attack
1. Reduction function(s) R
    * ?? i think they might be same func but with diff params?
    * R maps from hash-values to pre-image values
    * create pre-images given a hash-value
    * the hash-value is longer than the pre-image hence they are "reducers"
    * reduction functions aren't perfect so there can be false alarms when searching
    * QUESTION: what do the reduction functions look like?

Steps

1. Choose a set of starting pre-images
2. For each pre-image build a chain
   1. hash the pre-image, then call reducer function on it to get to another pre-image
   2. continue this successive application of H and R functions for the desired length of your chain
3. Store the first pre-image and the last pre-image and discard all other values
4. repeat for each starting pre-image you chose

Description

* Rainbow tables are a form of compression
    * Storing the first+final pre-image in the chain allow you to compute all the hash-values and pre-images on the chain
    * The length of the chain says how much compression you are applying. Longer chain => save more space but more work to do to walk the chain

#### Searching a rainbow table

Phase 1

Given an arbitrary hash value:

1. Assume that the pre-image is somewhere in your dataset.
1. you know it's the output of a H function **somewhere** in one of your chains
1. so if you apply R to it you get the next pre-image in that chain
1. and by continuing to "walk the chain forward"  of applying H and R in succession you will eventually get to the end of the chain and generate a pre-image that you have actually stored

Phase 2

* Go to the first pre-image in the chain and walk the chain forward until you find the hash-value you are searching for
* the pre-image that generated that hash value is the password you want

#### Their status in the industry

> https://www.csoonline.com/article/3623195/rainbow-tables-explained-how-they-work-and-why-theyre-mostly-obsolete.html
>
> From a modern password cracking threat perspective though, rainbow tables are
> mostly obsolete, and that’s not only due to the previously mentioned commonality
> of password salting that makes them ineffective. They have also long since been
> replaced by more advanced, powerful practices less hampered by limitations.
> “Rainbow tables rarely provide value compared to optimized GPU-based cracking,
> because they are very specific to a given password hash and password type, and
> they’re slow to generate,” JP Aumasson explains.
>
> Jeremi Gosney, founder and CEO of distributed password cracking company
> Terahash, concurs. “Modern password cracking is highly dynamic, and requires
> agility, flexibility, and scalability. Rainbow tables are static, rigid, and not
> at all scalable—they are the antithesis of modern password cracking. Even if you
> don’t have the horsepower of GPUs, employing modern techniques will still net
> you a far higher hash yield than rainbow tables will.” Probabilistic candidate
> generation, rules, hybrid attacks, and now even machine learning candidate
> generation are far superior to rainbow tables, he says.

### Modern password cracking tools

* Terahash will sell you a rack of GPUs to run their "hashstack" software
    * they are planning a free version
* hashcat
    * terahash also make the free `hashcat` tool which seems very comprehensive
* L0phtcrack
    * bought by Terahash but it fell through due to their supply chain -> cashflow woes
    * now open source
    * future unclear


## Background on password storage

* A _password hash_ is created by feeding the user's real password into an algorithm which generates the hash as output. The "hash" looks like a random string of characters
* The first part of the hash `$XXX$` identifies which algorithm was used, in this case `XXX`
    * is this **always** true or is it a bcrypt thing?
* hash functions are deterministic (they will always generate the same output for a given input)
    * -- an attacker can inspect the hashes in the DB and see which user's passwords are the same
        * salts mitigate this issue

## Salt

* a unique string (different for each user) which is stored in the DB alongside the password hash
* it stops the same user password from hashing to the same thing each time
* using salts means that password duplication doesn't appear as hash duplication in the database
* using salts mean that an attacker has to create a rainbow table for each hash+salt (slows them down)
    * salts are speed bumps to cracking the passwords - they don't prevent it!
* salts are stored in cleartext in the users table
* salts should be cryptographically random and sufficiently long
* salts can be prepended or appended to the password
* DO
    * use a Cryptographically secure pseudo-random number generator (CSPRNG)
    * use a "long enough" salt
* DO NOT use a system-wide salt (i.e. same salt for every password)
    * it just makes passwords longer
    * it allows attacker to continue to use hash tables
    * use usernames as salts

## Pepper

* pepper is a random string but it is not stored in the DB
    * can be stored in env of server or on disk - ideally somewhere with a security boundary between it and the DB so if an attacker pops one they can't get the other
* if an attacker gets your db (hashes and salts) but not the pepper then cracking the password is that much harder for them
* Where the salt only has to be long enough to be unique, a pepper has to be secure to remain secret (at least 112 bits is recommended by NIST), otherwise an attacker only needs one known entry to crack the pepper.
* even if the pepper is breached, an attacker still has to brute force the hashes i.e. the system falls back to the level of security you would have with just salt+hash
* -- if you lose the pepper secret then none of your users will be able to login ever again
* -- have to be careful not to exceed the 72 char limit for bcrypt. ALWAYS APPEND THE PEPPER. NEVER PREPEND IT lest the user's password get truncated away and all passwords become the pepper!
* a pepper can improve the strength of every users' password
    * only works if the DB is compromised but the app is not
* Introducing a pepper would be like changing the password for all users
* After turning on the pepper
    * you would need "legacy pepperless password verify" and "new peppered password verify" paths, sending users to the appropriate one based on some flag in the user model

## Aside: server relief

You can give some of the password hashing work to the client to do in JS. This is called "server relief" and libsodium supports it.

## Algorithms

### Bcrypt

how long is the salt in bcrypt
a bcrypt hash usually begins with the marker $2b$
* max password length is 72 chars, everything else is truncated
* seems like a safe but maybe old choice in 2022
* the config params matter (see tweet above)

### Argon2

* https://www.rfc-editor.org/rfc/rfc9106.html (informational RFC)
* a password hashing algorithm
* variants:
    1. Argon2i
        * safer against side-channels
    1. Argon2d
        * safer against GPU cracking
    1. Argon2id
        * a combo
* support:
    * PHP 7.2+ has built-in support in it's password_hash function but the default is still bcrypt
    * https://github.com/technion/ruby-argon2 ruby gem
    * libsodium
        * https://libsodium.gitbook.io/doc/password_hashing#:~:text=Argon2,with%20the%20exception%20of%20JavaScript.&text=Libsodium%20supports%20Argon2i%20and%20Argon2id.
    * it was the default hasher in Symfony 4 but they went back to bcrypt for 5.3 based on the tweet below

This post says you should use it: https://security.stackexchange.com/questions/193351/in-2018-what-is-the-recommended-hash-to-store-passwords-bcrypt-scrypt-argon2

This twitter thread says that it's not better than bcrypt

> https://twitter.com/TerahashCorp/status/1155129705034653698?s=20&t=8DTMb3RCVB1GPPcXe9AEzw
>
> It is weaker than bcrypt at runtimes < 1000 ms.
>
> But you shouldn't use Argon2 anyway, unless you are using it as a KDF instead of a PHF.
>
> Wasn't it specifically designed with that purpose? https://password-hashing.net
>
> It was. But me( @jmgosney) and @Sc00bzT were both on the experts panel for the Password Hashing Competition, and both of us will tell you not to use Argon2 for password hashing. It is weaker than bcrypt at runtimes < 1000 ms.
>
> Eh, we've ranted about it on Twitter a few times but it's honestly not that big of a deal, and in the end we're password crackers so pushing the issue isn't exactly in our best interest.
>
> If it were horridly broken or seriously weakened security in a very impactful way, then it would be a different story. But "argon2 isn't actually better than bcrypt" isn't exactly a headline.
>
> Bottom line, if you're already using argon2, you're totally fine. It's still a good PHF and much better than most everything out there. But if you aren't using argon2, bcrypt is a better choice.


## Real systems

### Devise

* uses bcrypt with a random salt (?? bytes) for each user
* defaults to 12 rounds for bcrypt
* they recommend at least 10
* supports a pepper
    * disabled by default
    * default value created by `SecureRandom.hex(64)` and hard-coded into `config/initializers/devise.rb`
    * the pepper is simply appended to the user's password before being passed to bcrypt
        ```ruby
        def self.digest(klass, password)
            if klass.pepper.present?
                password = "#{password}#{klass.pepper}"
            end

            ::BCrypt::Password.create(password, cost: klass.stretches).to_s
        end
        ```

### Drupal

* SHA-512 with salt (length ???)
* uses PhPass under the hood
* no pepper, cannot be easily added AFAICT
* drupal has some salt stuff in settings.php but AFAICT it's not for the password

> SHA-512 based phpass-like hash encoding strings use "$S$" as the hash type identifier.

### Wordpress

* https://developer.wordpress.org/reference/functions/wp_hash_password/
* uses `PasswordHash` under the hood
	* uses a salt and 8 rounds (256 passes) of MD5
	* uses md5 because it's supported on all platforms
	* you can configure it to use blowfish instead
* this can be changed with plugins

This doesn't seem like an optimal choice. Does pantheon mitigate this at all?

### Silverstripe

> Silverstripe CMS stores passwords with a strong hashing algorithm (blowfish)
> by default (see PasswordEncryptor). It adds randomness to these hashes via salt
> values generated with the strongest entropy generators available on the platform
> (see RandomGenerator). This prevents brute force attacks with Rainbow tables.
>
> https://docs.silverstripe.org/en/4/developer_guides/security/secure_coding/

https://api.silverstripe.org/4/SilverStripe/Security/PasswordEncryptor.html

* ++ uses random salts
* ?? does this mean they use bcrypt?
* ?? how many rounds

### Django

* https://docs.djangoproject.com/en/4.0/topics/auth/passwords/
* uses PBKDF2 with SHA256 by default because it doesn't require a 3rd party lib
* can be changed to bcrypt
* if you change the preferred storage algorithm, django can automatically upgrade users
	* you provide django a list of password hashing functions and it will use whichever function matches the current stored password.
	* if you change your preference for which hashing function should be used for storage django will rehash the users' password when they login
	* this is pretty cool.
