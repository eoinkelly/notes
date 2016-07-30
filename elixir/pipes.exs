# Pipes
# #####

# EXPR |> FUNC(A, B)
# becomes
# FUNC((EXPR), A, B)
# The following written using pipes ...
IO.inspect (1..20) |> Enum.map(&(&1 * &1)) |> Enum.filter(&(&1 < 100))

# ... would be this without them
Enum.filter(Enum.map(IO.inspect (1..20), &(&1 * &1)), &(&1 < 100))

