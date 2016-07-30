
# There are two type attributes
#
# 1. @type
# 2. @spec

@type adapter         :: {module, term}
@type assigns         :: %{atom => any}
@type before_send     :: [(t -> t)]
@type body            :: iodata | nil
@type cookies         :: %{binary => binary}
@type halted          :: boolean
@type headers         :: [{binary, binary}]
@type host            :: binary
@type int_status      :: non_neg_integer | nil
@type owner           :: pid
@type method          :: binary
@type param           :: binary | %{binary => param} | [param]
@type params          :: %{binary => param}
@type peer            :: {:inet.ip_address, :inet.port_number}
@type port_number     :: :inet.port_number
@type query_string    :: String.t
@type resp_cookies    :: %{binary => %{}}
@type scheme          :: :http | :https
@type secret_key_base :: binary | nil
@type segments        :: [binary]
@type state           :: :unset | :set | :file | :chunked | :sent
@type status          :: atom | int_status

@spec assign(t, atom, term) :: t
def assign(%Conn{assigns: assigns} = conn, key, value) when is_atom(key) do
  %{conn | assigns: Map.put(assigns, key, value)}
end
