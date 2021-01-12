# OpenSSL

* On Ubuntu
    * `/usr/lib/ssl` is mostly links to under `/etc/ssl`
    * all the openssl stuff is in `/etc/ssl`
* All certs are in `/etc/ssl/certs`
	* that is mostly soft links to stuff under
		* `/usr/share/ca-certificates/mozilla`
* `/usr/share/ca-certificates/mozilla`
	* contains just a bunch of .crt files