#!/bin/sh
exec scala "$0" "$@"
!#
// What does the shell preamble (bit above this line) do?

object HelloScript extends App {
  println("Hello from a scala shell script")
}

// Where does the main function come from?
HelloScript.main(args)
