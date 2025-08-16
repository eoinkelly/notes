# netstat

# show routes

```
netstat -nr
netstat -nral
netstat -nrs
```

- -n show network addresses as numbers (normally netstat trys to resolve names
  for them)
- -r shows routing tables
    - -ra adds "protocol-cloned" routes
    - -rl shows MTU column
    - -rs show routing stats

```
-r    Show the routing tables.  Use with -a to show protocol-cloned routes.  When -s is also present, show routing statistics instead.
    When -l is also present, netstat assumes more columns are there and the maximum transmission unit (``mtu'') are also displayed.

The routing table display indicates the available routes and their status.  Each route consists of a destination host or network and a
gateway to use in forwarding packets.  The flags field shows a collection of information about the route stored as binary choices.  The
individual flags are discussed in more detail in the route(8) and route(4) manual pages.  The mapping between letters and flags is:

1       RTF_PROTO1       Protocol specific routing flag #1
2       RTF_PROTO2       Protocol specific routing flag #2
3       RTF_PROTO3       Protocol specific routing flag #3
B       RTF_BLACKHOLE    Just discard packets (during updates)
b       RTF_BROADCAST    The route represents a broadcast address
C       RTF_CLONING      Generate new routes on use
c       RTF_PRCLONING    Protocol-specified generate new routes on use
D       RTF_DYNAMIC      Created dynamically (by redirect)
G       RTF_GATEWAY      Destination requires forwarding by intermediary
H       RTF_HOST         Host entry (net otherwise)
I       RTF_IFSCOPE      Route is associated with an interface scope
i       RTF_IFREF        Route is holding a reference to the interface
L       RTF_LLINFO       Valid protocol to link address translation
M       RTF_MODIFIED     Modified dynamically (by redirect)
m       RTF_MULTICAST    The route represents a multicast address
R       RTF_REJECT       Host or net unreachable
r       RTF_ROUTER       Host is a default router
S       RTF_STATIC       Manually added
U       RTF_UP           Route usable
W       RTF_WASCLONED    Route was generated as a result of cloning
X       RTF_XRESOLVE     External daemon translates proto to link address
Y       RTF_PROXY        Proxying; cloned routes will not be scoped

Direct routes are created for each interface attached to the local host; the gateway field for such entries shows the address of the out-
going interface.  The refcnt field gives the current number of active uses of the route.  Connection oriented protocols normally hold on
to a single route for the duration of a connection while connectionless protocols obtain a route while sending to the same destination.
The use field provides a count of the number of packets sent using that route.  The interface entry indicates the network interface uti-
lized for the route.  A route which is marked with the RTF_IFSCOPE flag is instantiated for the corresponding interface.  A cloning route
which is marked with the RTF_PROXY flag will not generate new routes that are associated with its interface scope.

```

QUESTION: The above indicates that routes are obtained and "held" somehow in the
kernel - how does this work? QUESTION: what is the deal with cloning routes?
