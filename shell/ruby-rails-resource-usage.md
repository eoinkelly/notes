# Ruby & Rails resource usage

* 80 MB seems to be a minimum for a bare-bones rails app
* up to 250 MB is apparently normal in production for real apps
* http://www.nateberkopec.com/2015/07/22/secrets-to-speedy-ruby-apps-on-heroku.html
* > or a typical mature Rails application, the app will use about ~250 MB in RAM once it’s warmed up.
* Rails code loading is different in dev and production
* rails will use more memory when you have hit various parts of the app
* it should level off after that
* ~100ms response time for requests is what the big sites aim for
* A user navigating around wants to get from page to page in <1 sec so counting network latency etc. 100ms seems like a great goal

## Running minimal ruby process on mac

```rb
# minimal program used to test bare ruby sized
loop do; end
```

```
Ruby 2.2.2. running minial program above takes up 4.8 MB memory (4.7 compressed) and starts 2 threads and opens 13 ports
    real: 2.1 MB
    private: 0 KB
    shared: 2.1 MB

Ruby 2.2.2 running rails server via webrick takes 83.3 MB memory, 2 threads, 16 ports
    real: 5.6 MB
    private: 486 KB
    shared: 3.9 MB
```
