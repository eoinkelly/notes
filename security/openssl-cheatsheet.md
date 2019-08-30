# OpenSSL Cheatsheet


```
$ openssl version -a
OpenSSL 1.1.1  11 Sep 2018
built on: Thu Jun 20 17:36:28 2019 UTC
platform: debian-amd64
options:  bn(64,64) rc4(16x,int) des(int) blowfish(ptr)
compiler: gcc -fPIC -pthread -m64 -Wa,--noexecstack -Wall -Wa,--noexecstack -g -O2 -fdebug-prefix-map=/build/openssl-cn9tZy/openssl-1.1.1=. -fstack-protector-strong -Wformat -Werror=format-security -DOPENSSL_USE_NODELETE -DL_ENDIAN -DOPENSSL_PIC -DOPENSSL_CPUID_OBJ -DOPENSSL_IA32_SSE2 -DOPENSSL_BN_ASM_MONT -DOPENSSL_BN_ASM_MONT5 -DOPENSSL_BN_ASM_GF2m -DSHA1_ASM -DSHA256_ASM -DSHA512_ASM -DKECCAK1600_ASM -DRC4_ASM -DMD5_ASM -DAES_ASM -DVPAES_ASM -DBSAES_ASM -DGHASH_ASM -DECP_NISTZ256_ASM -DX25519_ASM -DPADLOCK_ASM -DPOLY1305_ASM -DNDEBUG -Wdate-time -D_FORTIFY_SOURCE=2
OPENSSLDIR: "/usr/lib/ssl"
ENGINESDIR: "/usr/lib/x86_64-linux-gnu/engines-1.1"
Seeding source: os-specific
```

* On Ubuntu
    * /usr/lib/ssl is mostly links to under /etc/ssl
    * all the openssl stuff is in /etc/ssl
* All certs are in /etc/ssl/certs
	* that is mostly soft links to stuff under
		* /usr/share/ca-certificates/mozilla
* /usr/share/ca-certificates/mozilla
	* contains just a bunch of .crt files


## Task: verify a certificate chain

Given a `.cer` file containing a chain of certificates for a site, show that this machine can can verify them using it's built-in certificate store


```
# First split the file into multiple .cer files each with a single cert

# these commands must be run on the server you are verifying i.e. using the
# same openssl installation as the server software will use.

# get info about each .cer
openssl x509 -in file.cer -noout -subject
openssl x509 -in file.cer -noout -issuer


# verify from a given root cert
openssl verify -CAfile root_ca.cer -untrusted middle.cer my_server.cer

# verify using the store of root certs built into openssl
openssl verify -untrusted middle.cer my_server.cer
```

