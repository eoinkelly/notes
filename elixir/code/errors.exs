defmodule A do
  def run do
    IO.puts("starting run")
    IO.inspect(self(), label: "Main process PID")

    # pid = spawn_link(B, :raise_an_exeption, [])
    # pid = spawn_link(B, :do_a_throw, [])
    pid = spawn_link(B, :do_exit, [])
    IO.inspect(pid, label: "spawned pid")

    IO.puts("end of run")
  end
end

defmodule B do
  # Raising an exeption will stop the other process if they started with start_link
  def raise_an_exeption do
    raise "hello"
  end

  # This becomes an ErlangError in the linked process
  # (ErlangError) Erlang error: {:nocatch, "a ball"}
  # and that exception casues it to exit
  def do_a_throw do
    throw("a ball")
  end

  def do_exit do
    # this does nothing to the linked process
    # exit(:normal)

    # this causes the linked process to EXIT
    # exit(:boo)

    # In OTP this would be a normal exit but without OTP this is just a non :normal exit and is treated as such
    exit(:shutdown)
  end
end

A.run()
