# Pipes
# #####

# EXPR |> FUNC(A, B)
# becomes
# FUNC((EXPR), A, B)
IO.inspect (1..20) |> Enum.map(&(&1 * &1)) |> Enum.filter(&(&1 < 100))

