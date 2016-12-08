
Print debugging

println!("{}", x);

argument placeholders:

```
{} // requires std::fmt::Display trait be implemented for that type
{:?} // requires std::fmt::Debug trait be implemented for that type
```

println! uses format! internally

> When requesting that an argument be formatted with a particular type, you are
> actually requesting that an argument ascribes to a particular trait.
>
> This allows multiple actual types to be formatted via {:x} (like i8 as well
> as isize). The current mapping of types to traits is:
>
> nothing ⇒ Display
> ? ⇒ Debug
> o ⇒ Octal
> x ⇒ LowerHex
> X ⇒ UpperHex
> p ⇒ Pointer
> b ⇒ Binary
> e ⇒ LowerExp
> E ⇒ UpperExp
