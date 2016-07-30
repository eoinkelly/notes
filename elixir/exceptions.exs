# exceptions are defined similarly to how structs are

defmodule BadError do
  defexception message: "bad thing happened"

  @moduledoc """
  some docs on the exception
  """
end


def some_func do
  raise BadError
end

# Plug has a Plug.Exception protocol that adds a `plug_status: 400` to each exception
