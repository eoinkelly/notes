### If you wish to use extra libraries,
### add their flags in the "LIBS" variable (like we do with "CFLAGS").

###
CFLAGS  = -std=c99
CFLAGS += -g
CFLAGS += -Wall
CFLAGS += -Wextra
CFLAGS += -pedantic
CFLAGS += -Werror

VFLAGS  = --quiet
VFLAGS += --tool=memcheck
VFLAGS += --leak-check=full
VFLAGS += --error-exitcode=1

say_hello: 1_say_hello.out
	@./1_say_hello.out

memcheck: 1_say_hello.out
	@valgrind $(VFLAGS) ./1_say_hello.out
	@echo "Memory check passed"

clean:
	rm -rf *.o *.out *.out.dSYM

1_say_hello.out: 1_say_hello.c
	@echo Compiling $@
	@$(CC) $(CFLAGS) 1_say_hello.c -o 1_say_hello.out $(LIBS)

