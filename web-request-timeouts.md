# Web Request Timeouts

* It looks like browsers don't enforce a timeout (or Chrome doesn't at least)

I did the following test:

```
Rails dev env (so puma with nothing in front of it) with infinite loop in controller i.e. rails sends no response
Browser is google chrome stable.
Request keeps trying for at least 1000 sec (I stopped it after that)
```


nginx will kill the connection if it takes too long. We configure it as follows:

proxy_read_timeout 120;

> Defines a timeout for reading a response from the proxied server. The
> timeout is set only between two successive read operations, not for the
> transmission of the whole response. If the proxied server does not transmit
> anything within this time, the connection is closed.

send_timeout 120;

> Sets a timeout for transmitting a response to the client. The timeout is
> set only between two successive write operations, not for the transmission of
> the whole response. If the client does not receive anything within this time,
> the connection is closed.

proxy_send_timeout 120;

> Sets a timeout for transmitting a request to the proxied server. The
> timeout is set only between two successive write operations, not for the
> transmission of the whole request. If the proxied server does not receive
> anything within this time, the connection is closed.


Also the AWS load balancer will kill the connection after 60 seconds but can be configured to be between 1-3600 seconds
