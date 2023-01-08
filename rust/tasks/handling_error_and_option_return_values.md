
## dealing with a Result<a, b>

```rs
let f = File::open("log.txt"); // returns Result<File, Error>
let f = File::open("log.txt")?; // return File
let f = File::open("log.txt").expect("failed to open log");
```