# Calculating checksums
```bash
# NB the -n is very important, otherwise  you'll get a \n in the output
echo -n "hello" | shasum # SHA-1 by default
echo -n "hello" | shasum -a 256 # SHA-256
a
```

```ruby
[1] pry(main)> require "digest"
=> true
[2] pry(main)> Digest::SHA256.hexdigest("abcd")
=> "88d4266fd4e6338d13b845fcf289579d209c897823b9217da3e161936f031589"
[3] pry(main)> Digest::SHA1.hexdigest("1")
=> "356a192b7913b04c54574d18c28d46e6395428ab"
```
