# stty

- sets options for whichever terminal device interface is its STDIN

stty flags

```
$ stty

foo  # flag is on
-foo # flag is off
```

example normal output

```
$ stty -a
speed 38400 baud; 21 rows; 90 columns;
lflags: icanon isig iexten echo echoe echok echoke -echonl echoctl
  -echoprt -altwerase -noflsh -tostop -flusho pendin -nokerninfo
  -extproc
iflags: -istrip icrnl -inlcr -igncr -ixon ixoff ixany imaxbel iutf8
  -ignbrk brkint -inpck -ignpar -parmrk
oflags: opost onlcr -oxtabs -onocr -onlret
cflags: cread cs8 -parenb -parodd hupcl -clocal -cstopb -crtscts -dsrflow
  -dtrflow -mdmbuf
cchars: discard = ^O; dsusp = ^Y; eof = ^D; eol = <undef>;
  eol2 = <undef>; erase = ^?; intr = ^C; kill = ^U; lnext = ^V;
  min = 1; quit = ^\; reprint = ^R; start = ^Q; status = ^T;
  stop = ^S; susp = ^Z; time = 0; werase = ^W;
```

Flag meanings:

- icrnl = map CR to NL on input
