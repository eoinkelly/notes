/*
The : indicates namespacing
inside the {} is what happens in response to the event
*/
syscall:::entry {
  /* probefunc = name of system call */
  /* execname = the name of the executable that called it */
  @[probefunc, execname] = count();
}

/* the dtrace:::END event fires when you hit Ctrl-C after running dtrace */
dtrace:::END {
  /* truncate all the collected stuff to only the top 10 */
  trunc(@, 10);
  /* print out the collected stuff */
  printa(@);
}
