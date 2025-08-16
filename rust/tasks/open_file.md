## Read a whole file into memory as a String

```rs
let path = Path::new("../../inputs/day_1_part_1_input.txt");
let input = fs::read_to_string(path).expect("Failed to read the file");
```

## Read a file line by line

```rs
use std::fs::File;
use std::io::BufReader;

let file = File::open("../../inputs/day_1_part_1_input.txt")?;
let buf_reader = BufReader::new(file);

for line in buf_reader.lines() {
    dbg!(line.unwrap());
}
```

## Read a while file directly into a Vec<u8>

TODO: when would you do this instead of into a String ?

```rs
// read a file into a Vec<u8>
fn read_file() -> std::io::Result<Vec<u8>> {
 let mut file = File::open("arp.txt")?;
 let mut buf = Vec::new();
 file.read_to_end(&mut data);
 return Ok(data);
}
```
