# Misc erlang performance things

## https://vimeo.com/album/3642282/video/145492645

Don't let lots of messages build up in an process inbox _ erlang gives each
process a "reduction count" of ~2000 _ the process _sending_ messages to an
overwhelmned inbox will incur a "reduction count penalty" _ I think: this
shortens its time slice that the VM will give it _ it is a coping mechanism but
can cause the system to get sluggish

handling 10k+ messages per second in the system you will want to suppress
unnecessary messages

proc_lib:spawn

- ++ nice error reports if process crashes
- -- error reports are sent to the error_logger
    - error_logger is bad at handling high volue of error messages
- he recommends not using it in production if you have high volume of messages

Tip: use ETS table as a message queue

++ allows the consumer to pull when it is ready rather than having them forced
into its inbox

Erlang has a built-in overload control module

- ++ can be used as a template to write your own
- -- implemented as a gen_server so quite message have so ironically can get
  overwhelmned in high load
- -- caters for global load only, not interface specific loads
