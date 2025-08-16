# Annotations

- <https://docs.oracle.com/javase/tutorial/java/annotations/>
- provide metadata about the program
- Are essentially a way of providing "structured, easier to parse by machine,
  comments"
- are not part of the running of the program (but their values can be available
  at runtime depending on their `@Retention`)

Uses

1. info for the compiler
1. info for tools that can process code at compile time or deploy time e.g.
   generate code, XML etc.
1. runtime processing - some annotations can be inspected at runtime.

- There are some pre-defined annotations that come with `java.lang` are
    - `@Deprecated`
        - tells compiler to emit a warning if the thing is used.
    - `@Override`
        - tells compiler that you belive the method it annotates is overriding a
          method from a superclass
        - it is **not** required.
    - `@SuppressWarnings("category-name")`
    - `@SafeVarargs`
        - asserts that the method does not perform unsafe operations on its
          varargs argument
        - it suppresses warnings about varargs usage
    - `@FunctionalInterface`
        - added in SE8
        - indicates that the type defn is designed to be a "functional
          interface"
- A number of the predefined annotations are "meta annotations" i.e. they are
  annotating another annodation e.g.
    - `@Retention`
        - controls where the annotation is available (source only, compiled
          output, runtime)
    - There are other meta-annotations too - see
      <https://docs.oracle.com/javase/tutorial/java/annotations/predefined.html>
- You can also define your own annotations
    - See
      <https://docs.oracle.com/javase/tutorial/java/annotations/declaring.html>
    - seems to be mostly useful for providing structured comments which can be
      parsed by javadoc and other tools
- Annotations can be applied to declarations: declarations of
    1. classes
    1. fields
    1. methods
    1. and other program elements
- As of SE8 they can also be applied to types
- Java provides annotations to allow you to create a stronger type system on top
  of it
    > The Java SE 8 release does not provide a type checking framework, but it
    > allows you to write (or download) a type checking framework that is
    > implemented as one or more pluggable modules that are used in conjunction
    > with the Java compiler.
- Example: A compiler plugin could use the `@NotNull` annotation to ensure that
  the thing it annotates can never be `null`

```
new @Interned MyObject();
```

```
@Author(
   name = "Benjamin Franklin",
   date = "3/27/2003"
)


@SuppressWarnings(value = "unchecked")
@SuppressWarnings("unchecked")
```

Annotations can be name value pairs or just values

```
@ANNOTATION_NAME(ELEMENT_NAME = ELEMENT_VALUE, ...)
@ANNOTATION_NAME(ELEMENT_VALUE, ...)
```

You can have multiple annotations on the same delcaration

```
@Thing1
@Thing2(foo: "bar")
void doStuff() {
}
```

Repeating annotations are supported as of JavaSE 8 (their values accumulate)

```
@Thing2(foo = "bar")
@Thing2(blah = "baz")
void doStuff() {
}
```
