
Objections to using nil:

1. If your function normally returns instances of Foo but can return nil you are basically violating the contract of that method - nil is an instance of NilClass and does not respond to the same messages as your Foo instances. Use a null object here (a thing that is is "Foo like" enough to not force other coe that shouldn't care to care).
