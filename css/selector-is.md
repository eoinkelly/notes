# Newer selectors

- Spec https://www.w3.org/TR/selectors-4/

## matches any pseudo-class: `:is()`

- my conclusion: saves some typing, not super necessary in SCSS, can have a
  specificity gotchas re. specificity, required feature to make future CSS
  nesting work.

- spec https://www.w3.org/TR/selectors-4/#matches-pseudo
- `is:(.a .b .c)` will match any of the classes `a`, `b`, `c`
- useful for compound selectors
- Watch out for specificity. The specificity of :is() gets auto-upgraded to the
  most specific item in the list of arguments.
- if any selector in the list is not valid then that selector will be ignored
  but the whole list will not be
    - this "forgiving" selector behaviour is different to what selectors
      normally do

> The specificity of the :is() pseudo-class is replaced by the specificity of
> its most specific argument. Thus, a selector written with :is() does not
> necessarily have equivalent specificity to the equivalent selector written
> without :is()

Examples

```scss
is: (.out1 .out2 .out3) is(: .in1 .in2 .in3) {
    // some properties
}

// is the same as

.out1 .in1,
.out1 .in2,
.out1 .in3,
.out2 .in1,
.out2 .in2,
.out2 .in3,
.out3 .in1,
.out3 .in2,
.out3 .in3 {
    // some properties
}

// it is real pseudo selector so you can
div:is(.a .b .c) {
}

// which is same as:
div.a,
div.b,
div.c {
}
```
