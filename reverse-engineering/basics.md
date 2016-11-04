## Disassemblers

* GUI for mac
    * IDA - seems to be the Photoshop of disassemblers with a photoshop like
      price to go with it.
    * Hopper
        * seems good
        * cheaper than IDA (and works on mac)
        * free demo quite crippled
* Command line on Mac
    * `nm <binary>` - displays symbol table
    * `otool -t <binary>` show contents of `__TEXT` section as hex
    * `otool -vt <binary>` show contents of `__TEXT` and disassemble it (ATT syntax)
    * `otool -Vt <binary>` show contents of `__TEXT` and symbolically
      disassemble it (ATT syntax)
    * `/usr/local/bin/ndisasm -p intel -b 64 -a <binary>`
        * dumps much more than just the TEXT section
* Command line on Linux
    * ???
* Windows only
    * http://ollydbg.de/
    * https://www.hex-rays.com/products/ida/
