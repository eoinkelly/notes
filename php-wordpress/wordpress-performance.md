# Inbox

http://allinthehead.com/retro/361/how-to-make-your-website-fast
http://codex.wordpress.org/Testing_WordPress_Performance

who controls the things that effect performance: hosting provider connectivity
to the wider Internet hardware that the server software is running on how many
other users competing for same resources on that physical server end-user their
connection & latency to the Internet how fast their device can render stuff, jun
JS etc. wordpress-devs how efficient wordpress core is at using PHP, Mysql
me+plugn authors how efficiently we use available resources how many database
queries per page, how intensive are they?

Comparing shared hosting need to install the same wordpress files and database
on multiple shared hosting accounts and then run some speed tests. how do we
make sure that the timings aren't influenced by client's computer & connection?

real performance vs percieved performance

As soon as you’ve identified the user with a cookie (including something like a
PHP session, which of course uses cookies) then the request will hit your
backend web server. Unless configured otherwise (as we have) that would include
things like Google Analytics cookies, which of course, would be every request
from any JavaScript-enabled browser. If you static assets (images, CSS,
JavaScript) from the same domain, by default the cache will be blown on those,
too, as soon as a cookie is set. So you have to design for that.
