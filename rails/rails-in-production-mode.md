```
# Steps to run app in production mode locally
$ RAILS_ENV=production bundle exec rake assets:precompile

# Run default server in production mode
$ rails server -e production


To run whatever server is in Procfile
$ foreman start
# it seems foreman connects on localhost:5000


# REMEMBER TO CLEAN ASSETS BEFORE YOU RUN IN DEV MODE AGAIN

# To clean out old assets (it *cleans* assets and *clear* log files)
$ rake assets:clean
```
