=======================

PID
USER
PR
NI
VERT
    * The virtual memory space. The sum of everything in the virtual memory map
RES
    * Resident Set Size
    * The number of pages that are currently resident in RAM
    * The portion of the virtual memory space that is actually in RAM
    * On a lightly loaded system this might include pages not actually being used as linux doesn't reclaim them agressively if it doesn't need them
    * An ever-growing RSS (Resident Set Size) might indicate a memory leak
SHR
    * The amount of *resident* memory that is shared with other processes
S
%CPU
%MEM
TIME+
COMMAND

On Windows the "Working Set" metric roughly = SHR + RES on linux