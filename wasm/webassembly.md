# WebAssembly ecosystem

- [WebAssembly ecosystem](#webassembly-ecosystem)
  - [Overview](#overview)
  - [File formats](#file-formats)
  - [Interested parties](#interested-parties)
  - [Books about WebAssembly](#books-about-webassembly)
  - [Languages which can create wasm](#languages-which-can-create-wasm)
  - [Languages which are specific to wasm](#languages-which-are-specific-to-wasm)
  - [WebAssembly runtimes](#webassembly-runtimes)
  - [Places you can run wasm now](#places-you-can-run-wasm-now)
  - [Places you can maybe run wasm in future](#places-you-can-maybe-run-wasm-in-future)
  - [Reasons why wasm could be great](#reasons-why-wasm-could-be-great)
  - [WASI](#wasi)
    - [Missing in WASI](#missing-in-wasi)
    - [Low-level stuff](#low-level-stuff)
  - [Wit](#wit)
  - [Questions](#questions)
    - [Are dynamic langs realistic in wasm? if yes, then which ones](#are-dynamic-langs-realistic-in-wasm-if-yes-then-which-ones)
    - [it seems like networking in WASI is not implemented yet so current network access is a bit of hack](#it-seems-like-networking-in-wasi-is-not-implemented-yet-so-current-network-access-is-a-bit-of-hack)
    - [What is the state of using wasm from JS?](#what-is-the-state-of-using-wasm-from-js)
      - [Option: QuickJS](#option-quickjs)
      - [Option: SpiderMonkey](#option-spidermonkey)
    - [Misc questions](#misc-questions)
  - [Update 2023-01-23](#update-2023-01-23)
  - [Javy](#javy)
  - [Wizer](#wizer)
  - [Update 2023-02-11](#update-2023-02-11)
  - [Update 2023-03-04](#update-2023-03-04)

## Overview

-   Sources
    -   Spec:
        -   https://webassembly.github.io/spec/
        -   https://webassembly.github.io/spec/core/index.html
    -   https://bytecodealliance.org/
    -   https://wiki.alopex.li/ActuallyUsingWasm
    -   https://docs.wasmtime.dev/
-   WebAssembly
    -   a low-level abstraction over multiple chip ISAs
    -   File extensions
        -   `.wasm`
        -   `.wast`
-   Wasmtime
    -   a production ready WebAssembly virtual machine
    -   written in Rust
    -   seems to be a 13MB statically compiled binary on macOS
    -   parts
        -   [Cranelift](https://github.com/bytecodealliance/wasmtime/tree/main/cranelift)
            -   translates target independent IR to machine code
            -   has three backends: x86-64, aarch64 (aka ARM64), and s390x (aka IBM Z)
            -   On x86-64, Cranelift supports both the System V AMD64 ABI calling convention used on many platforms and the Windows x64 calling convention.
            -   On aarch64, Cranelift supports the standard Linux calling convention and also has specific support for macOS (i.e., M1 / Apple Silicon).
            -   > The speed of Cranelift's generated code is ~2% slower than that of V8
                > (TurboFan), and ~14% slower than WAVM (LLVM). Its compilation speed, in
                > the same paper, is measured as approximately an order of magnitude
                > faster than WAVM (LLVM). We continue to work to improve both measures.
    -   Rust compiler has a cranelift backend which will generate code for use in wasmtime
        -   https://github.com/bjorn3/rustc_codegen_cranelift
-   You can use wasm/wat modules from Ruby https://github.com/bytecodealliance/wasmtime-rb
-   road map (from https://www.youtube.com/watch?v=nVrQ5QgoCz8&t=1258s)
    -   gen 1 (shipped) support low-level languates
    -   gen 2: support high-level languages
    -   gen 3: support dynamic languages
        -   nobody actively working on this yet (in 2022)
        -   this is a "maybe" on the roadmap
        -   this would include JIT support of some kind
    -   they are using an evergreen model for the spec
        -   they have a live spec doc which keeps updating
    -   WebAssembly 1.1 will be rebranded as 2.0 for administrative reasons with the W3C
    -   from now on they will bump a minor version whenever they land a proposal
    -   strong backwards compatibility
    -   Standardisation process for wasm specs
        -   phase 1
        -   phase 2
        -   phase 3
            -   ready for people to start trying it
            -   you must create a prototype
            -   you must create a test suite
        -   phase 4
            -   finishing touches
            -   write a reference implementation in OCaml
            -   must have 2 implementations in 2 separate production engines
        -   phase 5
            -   W3C rubber stamps it

## File formats

-   .wasm (WebAssembly binary format)
    -   compact binary format
-   wat (WebAssembly text format)
    -   human readable/?writable wasm
    -   basically a re-transcription of the binary format in text
-   wast
    -   https://github.com/WebAssembly/spec/tree/master/interpreter#s-expression-syntax
    -   > a WebAssembly AST given in S-expression syntax
    -   WAST is a superset of the WebAssembly text format and not officially in the spec.
    -   It is used only for testing purposes, the reference implementation of
        WebAssembly uses this format for its test (mostly because it's easier to
        write by hand).
    -   examples
        ```wat
        (module
          (func $add (param i32) (param i32) (result i32)
            (get_local 0)
            (get_local 1)
            (i32.add)
          )
          (export "add" (func $add))
        )
        ```

You can convert between wat and wast

## Interested parties

1. Bytecode alliance

-   non-profit to promote using wasm outside of the browser
-   many members incl. AWS, Google, Mozilla, Shopify

2. W3C

-   rubber-stamping the spec when it's done

3. Browsers

-   include a wasm runtime to run wasm from JS

4. Wasmer

-   a startup who make wasmer runtime and the wapm package repository
-   seems to be a play to be the "docker of wasm"

5. Startup: https://www.fermyon.com/wasm-languages/webassembly-language-support

-   trying to create a serverless cloud you run wasm in
-   A play to make a lambda competitor which focuses on wasm and has great tooling for it

## Books about WebAssembly

-   WebAssembly in Action https://livebook.manning.com/book/webassembly-in-action/chapter-11/11

## Languages which can create wasm

Currently only statically compiled languages are supported. There is early work on a JS VM for wasm but it's still super early

-   Rust
    -   probably the most mature support
-   AssemblyScript https://www.assemblyscript.org/
    -   a typescript alike language which generates wasm
    -   shopify let you write modules in it to run in their wasm runtime
-   C & C++
    -   https://docs.wasmtime.dev/wasm-c.html
    -   llvm has backends (32 and 64) for wasm
    -   some stuff missing because no longjmp and no stack unwinding so no C++ exceptions yet
    -   you can use clang https://depth-first.com/articles/2019/10/16/compiling-c-to-webassembly-and-running-it-without-emscripten/
    -   gcc also supports it https://www.wasm.builders/kirteeprajapati/say-hello-to-webassembly-with-cc-2lnd
-   Go
    -   https://binx.io/2022/04/22/golang-webassembly/
    -   https://docs.google.com/document/d/131vjr4DH6JFnb-blm_uRdaC0_Nv3OUwjEY5qVCxCup4/preview#heading=h.mjo1bish3xni
    -   Apparently it is annoying easy to write go code which will pull in a JS dependency when compiled to wasm because go's wasm support assumes a browser environment.
    -   presumably it also compiles the Go runtime into the wasm - does that make it very big?
    -   I think it works via tinygo at the moment, will it ever make sense to use "full" go? (TODO)
-   .Net (C#, F#)
    -   apparently well supported but I haven't verified

Language support is always a bit squirrelly because Wasm is a VM that is still evolving.

## Languages which are specific to wasm

-   AssemblyScript
-   Grain
-   Motoko

## WebAssembly runtimes

There are 20+ wasm runtimes but the key ones are:

-   Wasmtime
-   Wasmer https://github.com/wasmerio/wasmer
    -   Claims to be faster than wasmtime
    -   is a startup
    -   seems like there might be some drama between wasmer and bytecode alliance https://news.ycombinator.com/item?id=24900186
    -   they have https://wapm.io/ which seems to be an attempt to have a package manager for wasm packaged apps and libraries
    -   has a focus on integrating existing libraries e.g. ruby https://github.com/wasmerio/wasmer-ruby
-   WasmEdge https://github.com/WasmEdge/WasmEdge

## Places you can run wasm now

-   Fastly Compute@Edge
    -   https://docs.fastly.com/products/compute-at-edge
    -   You create wasm files and push them to Fastly
    -   They don't provide a higher level lang for you to write in
-   Shopify
    -   https://shopify.engineering/shopify-webassembly
    -   You can use anything which compiles to wasm but they are pushing AssemblyScript
-   Cloudflare workers
    -   you create the wasm any way (Rust, C, C++ etc.) you want and then push it to CF via Wrangler
    -   **workers can also run JS which runs in a VM Isolate not in wasm**
        -   https://developers.cloudflare.com/workers/learning/how-workers-works/
-   AWS Lambda
    -   Not an official AWS thing, but people have made wasm run on Lambda
    -   Uses https://github.com/WasmEdge/WasmEdge
        -   seems you have to run the wasm under WasmEdge which is run by NodeJS (all in a docker image)
            -   so you are probably better off running plain node JS tbh
    -   https://wasmer.io/posts/wasm-on-amazon-lambda-lessons-learned
        -   from Jun 2022
        -   seems like wasm on lambda is messier than JS on lambda or straight rust on lambda
-   from your ruby/python/js code
    -   ruby (and python etc.) can invoke a wasm runtime with wasm module
    -   TODO: how much slower/more memory intensive would this be vs invoking a normal C/rust shared lib?
        -   ++ it would remove the build step which would make installation easier
    -   ?? could this come to replace C deps in ruby gems/npm packages etc.? unclear.
-   On docker-desktop
    -   https://docs.docker.com/desktop/wasm/
    -   uses WasmEdge runtime under the hood
    -   this seems to be a play to be the "package manager" for wasm files
    -   it is useful to be able to transparently use your existing docker infra to run wasm
    -   it will be more useful when k8s and ECS support it
    -   TODO: presumably the wasm files are wrapped in a thin docker image?
        -   your wasm binary might need other files and to able to write stuff to the container filesystem (via the wasm runtime of course) so wrapping in a docker image seems sensible
-   On Azure managed k8s
    -   https://learn.microsoft.com/en-us/azure/aks/use-wasi-node-pools
    -   uses https://github.com/deislabs/containerd-wasm-shims under the hood which in turn uses wasmtime
    -   you can create a pool of nodes which run wasm wrapped in a docker image
        -   TODO: anything special about this wrapping?
-   MS Flight Simulator allows mods written in wasm
-   Cloud native space
    -   Envoy allows you to use plugins to transform requests in wasm
    -   Open policy agent allows you to use plugins to transform requests in wasm
    -   Data processing pipeline apps are also excited about this
    -   these are examples of "near data processing"
-   WasmCloud
    -   https://wasmcloud.com/
    -   has "location transparency" - you can run all your actors on one host during testing and spread out in prod

## Places you can maybe run wasm in future

-   Within database engines e.g.
    -   postgres plugin written in wasm https://medium.com/wasmer/announcing-the-first-postgres-extension-to-run-webassembly-561af2cfcb1
    -   (wild speculation) this would be easier if PG included a wasm runtime
-   Within any other large piece of software which wants to allow plugins
-   Serverless use-case
    -   A serverless env that provides the wasm runtime and you just ship wasm to it
    -   Fastly and CloudFlare are already doing this
    -   Will AWS/Google/MS do it? Why would they?
        -   ++ it makes cross-compiling a bit easier (meh)
        -   -- it's slower than native code (but not by a lot)
        -   ++ it is much easier to secure vs native processes in a multi-tenant environment
        -   ++ you could potentially offer the service for much cheaper because the isolation overhead would be cheaper

## Reasons why wasm could be great

-   Being able to call seamlessly between modules written in different languages would be an amazing step forward
    -   Is this going to be seamless in practice? Rust and C will likely work well but will a golang GC in one module play nicely with a python GC in a another module?
-   ++ if a platform supports wasm, you can write the code in anything which supports it
    -   -- realistically for near future that will be a compiled lang (Rust, C, C++ ?go)
    -   ++ they are working on making JS run well as wasm but it will always probably be slower than straight node but might be worth it for the security benefits
-   wasm code is the most sandboxed you can make 3rd party code
-   if the container ecosystem can use wasm processes as a drop in replacement for containers (keeping the config the same for both e.g. similar yaml in k8s and docker desktop) then wasm "containers" just become a 1)more sandboxed, 2) faster startup (depends), 3) cross-platform alternative to a natively compiled workload
-   "serverless without a cold boot problem"
    -   because wasm code is so much more dependent on the host, you can think of it as being a "business logic" plugin to the wasm runtime
-   Use cases
    -   Plugins
        -   it could be the "the last plugin language you will ever need"
-   Security
    -   the granular security & sandboxing could be be a big security win
    -   ??
    -   the capabilities of a module are in a cryptographically signed header so can be checked even if there is no network
    -   no calling home to a central server required

## WASI

-   List of proposals: https://github.com/WebAssembly/WASI/blob/main/Proposals.md (<-- Get update on progress here)
-   WASI is an abstraction over multiple system call interfaces (Linux, Windows etc.)
-   includes low level stuff like files, sockets etc.
-   high level system interfaces like crypto and ML
-   a choice was made to not give direct access to a real filesystem
-   https://wasi.dev/
-   WASI standard is being developed as a set of separate proposals
    -   each of which advances through stages akin to the Ecmascript process
    -   As of Dec 2022
        -   nothing in phase 3 or later
        -   lots in phase 1 and 2
-   WASI filesystem options:
    1. a "legacy filesystem interface"
    -   which is a posix alike API
    -   you can compile you existing "legacy" code to use this interface
    -   you can use this on
    1. compatibility layer
       bakes the files into the wasm binary
    1. WASI IO API
    -   passes around streams of bytes not files
    -   these streams are "IO types"
-   Different WASM modules can use different interfaces so you can do gradual adoption
-   wasi sees a file = data_bytes + filesystem_metadata
-   compared to posix APIs
    -   traditional world
        -   process gets a file path as a string
        -   uses the open syscall to get a handle from the file and can read the stream of bytes
    -   wasm instead takes a stream
        -   the OS is responsible for opening the file, getting an handle and giving the wasm a stream of bytes

### Missing in WASI

-   polyfilled by wasix lib:
    -   no pthreads or pthread stubs
    -   no BSD sockets API
    -   no user/group permissions API
-   no dup()
-   limited call stack size
-   emulated and limited signal support
-   ENOTCAPABLE

### Low-level stuff

-   wasm is a Harvard architecture not a Von-Neumann architecture
    -   it has no code pointers
    -   data and code travel on separate pathways
-   the callstack is not accessible
-   https://en.wikipedia.org/wiki/Harvard_architecture

## Wit

Wit is the interface definition format for wasm component modules
I believe there are \*.wit files which define it
https://github.com/bytecodealliance/wit-bindgen can auto generate these files from Rust traits you write

## Questions

### Are dynamic langs realistic in wasm? if yes, then which ones

-   bytecode alliance is working on making a JS VM that runs in wasm
    -   https://bytecodealliance.org/articles/making-javascript-run-fast-on-webassembly
    -   currently impossible to have a JIT in a wasm VM so it might never be as fast as V8
        -   nothing I have read says that a JIT would ever be allowed - seems like it would be a big security hole
        -   but maybe still fast enough?
    -   the same approach could be used for other dynamic langs
        -   but will anyone put in that much work?
    -   wasm doesn't support GC yet but that is planned
    -   bottom line: you need a whole new interpreter/VM for your language
    -   CloudFlare have gone with V8 isolates for their JS workers
        -   that might be a better long-term strategy for running JS because prod quality JS VM in wasm could be quite far away

### it seems like networking in WASI is not implemented yet so current network access is a bit of hack

-   WAGI is a hack which takes a CGI like approach to networking
-   there is no official WASI networking support so all these workarounds are subject to change

### What is the state of using wasm from JS?

```
What can be transferred across the boundary? What types?
  I believe it's only numbers right now
How slow is the boundary crossing?
What is it being used for in production right now?

wasm modules cannot connect to each other yet (waiting on the component model)
  so all your wasm modules a little islands that can only talk to JS and not
  directly to each other
```

#### Option: QuickJS

https://bellard.org/quickjs/

> QuickJS has many virtues, the foremost being that it is fully compliant to the
> latest JavaScript standard. But it is not designed to be the fastest or most
> robust JavaScript engine. It is so easy to embed that many early Wasm projects
> have figured out ways to compile the interpreter to Wasm and then run JavaScript
> inside of a Wasm runtime.
>
> https://thenewstack.io/webassembly-5-predictions-for-2023/

#### Option: SpiderMonkey

> we’ve added full support to Mozilla’s build system for compiling SpiderMonkey
> to WASI. And Mozilla is about to add WASI builds for SpiderMonkey to the same CI
> setup that is used to build and test Firefox. This makes WASI a production
> quality target for SpiderMonkey and ensures that the WASI build continues
> working over time.
>
> https://bytecodealliance.org/articles/making-javascript-run-fast-on-webassembly

-   -- you need to pre-initialize your JS with wizer to run it in this spidermonkey
    -   https://github.com/bytecodealliance/wizer

### Misc questions

-   Is the future of server side software us writing plugins for runtimes and managed services?
    -   Do these plugins need to respond to a single HTTP request?
    -   can a rails app be thought of as a plugin? Not really, it's more of an orchestrator/controller
    -   a plugin would be where the routing later is taken care of by the cloud for you
-   What would a "wasm container" be?
    -   you don't need to provide a full OS underneath it because the compiled wasm doesn't care about it
    -   it could be a smaller more secure container
-   I think my conclusion is that for 2023 at least, wasm is in the interesting but not useful category

## Update 2023-01-23

-   https://github.com/wasmerio/kernel-wasm
    -   definitely potentially faster
    -   but also scary. Any bug in the wasm runtime is presumably now kernel pwnage?
    -   my take:
        -   interesting for niche cases but the security trade-offs likely too scary for general purpose use
        -   no hosting provider will want to run my wasm in their kernel
        -   might be useful for "more expressive eBPF"? (low confidence, lots I don't understand about the details there)
            -   but eBPF exists so what does this add?
        -   will only be of interest where the entity who runs the kernel also supplies the wasm

## Javy

1. take in raw JS as input
1. bake the raw JS into a wasm file with the QuickJS interpreter
1. Run the wasm through wizer to translate the JS code into QuickJS bytecode and take a snapshot
1. Output a wasm module with QuickJS interpreter + the bytecode of your JS code

Currently creates wasm modules which are 800KB+

You can remove bits of the QuickJS runtime to make it smaller. Not sure how manual that is?

This toolchain will evolve a lot as WASI evolves.

## Wizer

-   https://github.com/bytecodealliance/wizer
-   pre-initializes webassembly modules
    -   runs the module and figures out module state:
        -   values of global variables
        -   non-zero regions of memory
    -   the rewrites the wasm binary replacing the old data segments with the recorded data segments
-   improves start up latency

## Update 2023-02-11

-   https://shopify.engineering/javascript-in-webassembly-for-shopify-functions
    -   uses Javy as JS to WASM toolchain
    -   uses QuickJS as their JS interpreter
    -   all this could change when the component model proposal lands which would allow for dynamic linking in WASI
    -   you JS reads request from STDIN and outputs to STDOUT
        -   they have a "Javy runtime" which puts some sugar around this for your JS - there is a `Javy` global.
    -   The JS event loop is disabled!

## Update 2023-03-04

- https://github.com/bytecodealliance/wasm-micro-runtime
- https://surma.dev/things/rust-to-webassembly/index.html
  - Good tutorial