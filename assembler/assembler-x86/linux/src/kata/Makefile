# Standard practice is to put the most general targets first and the dependent
# targets leter
all: hello_world.o
	ld -o hello_world hello_world.o 

hello_world.o:
	yasm -f elf64 -g dwarf2 -l hello_world.lst -o hello_world.o hello_world.s

clean:
	rm -f hello_world hello_world.o hello_world.lst
