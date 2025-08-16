- The connection object in phoenix
- part of Plug

```
# Handy methods
from_private(conn, :key_name)
request_path(conn)
full_url(conn, request_path(conn), opts)
put_flash(conn, :key, "value")
get_session
```
