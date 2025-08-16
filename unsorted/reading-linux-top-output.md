=======================

PID USER PR NI VERT _ The virtual memory space. The sum of everything in the
virtual memory map RES _ Resident Set Size _ The number of pages that are
currently resident in RAM _ The portion of the virtual memory space that is
actually in RAM _ On a lightly loaded system this might include pages not
actually being used as linux doesn't reclaim them agressively if it doesn't need
them _ An ever-growing RSS (Resident Set Size) might indicate a memory leak SHR
* The amount of *resident\* memory that is shared with other processes S %CPU
%MEM TIME+ COMMAND

On Windows the "Working Set" metric roughly = SHR + RES on linux
