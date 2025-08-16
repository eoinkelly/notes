How fast can I make fizzbuzz? Output data measured using
[pv](https://www.ivarch.com/programs/pv.shtml) on my laptop

```bash
# Ruby attempt_1
$ cd ruby
$ ruby fizz_buzz.rb | pv -B 1024 > /dev/null
# 1.99GiB 0:01:07 [31.3MiB/s]


# Rust
$ cd rust/fizz_buzz
$ cargo build -r

$ ./target/release/fizz_buzz | pv -B 1024 > /dev/null
#  786MiB 0:00:54 [15.8MiB/s] [
```

WTF my rust version is slower?
