```bash

# if you have multiple redis instances you need to add the name as a suffix to the command e.g.
HEROKU_APP=my-app heroku redis:info # read the name of the redis instances from the output (only required if you have 2+ instances)
HEROKU_APP=my-app heroku redis:cli redis-shaped-1234
# not
HEROKU_APP=my-app heroku redis:cli



HEROKU_APP=my-app heroku redis:cli
HEROKU_APP=my-app heroku redis:credentials


# connect manually (without leaking the creds into your shell history)
# export MY_REDIS=$(HEROKU_APP=my-app heroku redis:credentials)
# I couldn't get this to work, it complained about 'AUTH Failed' even though password is in the redis url
```
