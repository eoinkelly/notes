# Check that some software verfies server SSL certs

## Strategy

1. edit /etc/hosts to make the software think that that the API endpoint it
   needs is `localhost
1. Run a simple HTTPS server on localhost:443 with the endpoints that your
   software needs to hit
1. Run your softare, see if it fails or passes

## Steps

1. `make_keys.sh` to create server key and server cert
1. `ruby server.rb` to run the sinatra server
1. `sudo vim /etc/hosts` to fool your client into thinking the API is local
1. test that you can hit the server with curl:
    ```
    curl --insecure -v https://my.api/some_endpoint
    ```
    This lets you verify that you have completed the previous steps correctly
1. Use the software you are trying to test to hit the API and see if it fails
   (as it should) because you are trying to fool it into sending traffic to your
   fake server rather than the real API
