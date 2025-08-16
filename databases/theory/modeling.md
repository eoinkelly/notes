# A short talk

# Clearly expressing database models

We often have reason to draw and discuss databas schemas on whiteboards etc.

Common terms are

In discussion "has many" "has one"

In drawing

(arrow with filled in head) line with 1 or _ at either end line with nothing at
one end and _ at the other

but these are very ambigious

(show some diagrams and see if everybody interprets the same) (start with posts
and authors then move to A and B to demonstrate how much of it depends on
knowledge of the domain which only _might_ be there)

So "has one" can mean has 0 or 1 has exactly 1

and "has many" can mean has 0 to many has 1 to many

## A better way: defining a relationship

- a relationship always has a direction
- relationships appear in pairs (every relationship has an "inverse
  relationship" which goes in the opposite direction

So instead of saying "what is the relationship between Author and Post" we say
"There are a pair of relationships between Author and Post. What are they?"

So when we want to draw how two boxes are connected we know we are actually
drawing _a pair_ of relationships.

## A better notation

Where N > 1 (and it is worth putting that on your diagram)

Kinds of relationship:

- 0-1
- 1 (or "exactly 1" if you prefer to be more explicit)
- 0-N
- 1-N

I think we need to abandon `*` to mean "many" - it is ambigious. `*` in reg exps
means 0-many but it's not clear in data modelling whether it also means 1-many

Remember relationships come in pairs so

```
Author has a 0-N with Post
Post   has a 1   with Author
```

So now we are able to clearly think about and communicate our data model on a
whiteboard. What about modelling it in software?

# Rails

- 0-1 `has_one`
- 1 ???
- 0-N `has_many`
- 1-N ???

How many people in the room are a bit surprised by that?

How can we model true `exactly 1` or `1-N` in rails?

- we can do it with validations
- BUT you should think of rails validations as "advisory" rather than
  "enforcing"
- If we want to "enforce" a relationship we need to do it in the database
- Enforcing `exactly 1` is awkward - see my notes on postgres true 1-1

# Sources

"All for One, One for all" paper by C.J. Date
http://www.dcs.warwick.ac.uk/~hugh/TTM/AllforOne.pdf
