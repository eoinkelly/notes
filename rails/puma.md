# Puma

The puma process converts the HTTP request from the socket to a rack ENV.
It is the rack ENV which is passed to theads in the thread pool

Puma does not use Ruby's stdlib openssl - it has it's own SSL implementation called mini_ssl
    they want to remove it but haven't yet
    mini_ssl does use OpenSSL under the hood (just not the Ruby OpenSSL)