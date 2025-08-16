### Advanced Ruby Video

#### My review:

- goes through some basic ruby stuff
- i don't agree with his explanation of duck typing - he seems to be mixing it
  up with hetrogenous arrays

#### Questions for rose

- Explain "duck typing" (don't just go off his (not great) explaination in the
  video)
- How many types can an object have in ruby? (hint: "type" in ruby kinda means
  "role") - see POODR for more info
- What does REPL stand for?
- Why does ruby have symbols? Every other language uses strings just fine?
- Why do we create an #initialize method but never call it? Why do we never put
  a #new method in the class file?
- When you print a class to the console in IRB (or rails console) what do you
  see between the `<>`?
- What are good reasons for re-opening a class?
- Does ruby have destructors?
- What is the naming convention about method names that end in `?`
- He uses inheritance here - when are times that is bad to use inheritance?
  could we use modules to achieve the same thing?
- Advanced: How does the xs.each(&:foo) work?

Q: How does `&:foo` create a reference to a method? A: & calls `#to_proc` on the
item, how else can I make use of `#to_proc`? TODO: understand this better
