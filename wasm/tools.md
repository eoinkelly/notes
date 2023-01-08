

```sh
# via brew
wasmtime --version

# via cargo install
wasm-tools --version

# via npm
npx wat2wasm --version
# other tools also installed - see https://www.npmjs.com/package/wabt


at 09:25:34 ❯ wasm-tools --help
Usage: wasm-tools <COMMAND>

Commands:
  parse      Parse the WebAssembly text format
  validate   Validate a WebAssembly binary
  print      Print the textual form of a WebAssembly binary
  smith      A WebAssembly test case generator
  shrink     Shrink a Wasm file while maintaining a property of interest (such as triggering a compiler bug)
  mutate     A WebAssembly test case mutator
  dump       Debugging utility to dump information about a wasm binary
  objdump    Dumps information about sections in a WebAssembly file
  strip      Removes custom sections from an input WebAssembly file
  compose    WebAssembly component composer
  demangle   Demangle Rust and C++ symbol names in the `name` section
  component  WebAssembly wit-based component tooling
  help       Print this message or the help of the given subcommand(s)

Options:
  -h, --help     Print help information
  -V, --version  Print version information
```