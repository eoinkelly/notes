# vmmap (macOS equivalent of linux pmap)

- has very detailed output

# Activity monitor (Mac only)

Good resource on how memory works on MacOS:
http://stackoverflow.com/a/1954774/356025

What is mac memory compression?

In activity monitor:

- Mac Private memory RPRVT
    - stack memory + malloc()'d memory (it may be shared with a forked child as
      long as copy-on-write has not been triggered on it by a write from either
      parent or child)
- Mac Shared memory RSHRD
    - memory that is currently visible in the address space of multiple
      processes
    - and memory that is likely to be shared e.g. the TEXT segment (binary code
      of the app)
- Mac Real memory RSIZE
    - Amount of RAM currently assigned to the process (incl. private and shared)
    - can be bigger than private+shared if process has more mem assigned to it
      than it is currently using (kernel can quickly take it back if it needs
      to)
    - can be smaller than private+shared if process has requested and received
      virtual memory but it has not yet used it so kernel has not put those
      pages into RAM
    - Seems to be roughly RSS in linux world

Wired memory = memory that must never be paged to disk e.g. kernel stuff
