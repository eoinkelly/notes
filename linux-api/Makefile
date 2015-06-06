# http://www.gnu.org/software/make/manual/make.html

test : test.c
	cc test.c -o ./bin/test

glibc-version : glibc-version.c
	cc glibc-version.c -o ./bin/glibc-version

# clean is a "phony target" as it is not the name of a file
clean :
	rm ./bin/test ./bin/glibc-version
