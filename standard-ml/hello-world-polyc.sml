(*
 * ############# polyc hello world #################
 *
 * * polyc requires a `main` function as entry point
 * * this doesn't work in MLton because nothing is evaluated at the top level so no output
 *
 * Build this with:
 *
 *     $ polyc -o hello-world ./hello-world-polyc.sml
 *
 *)
fun hello () =
  print "Hello, World!\n"

fun main () = hello ()
