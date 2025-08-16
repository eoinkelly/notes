# Propositonal logic

https://www.tutorialspoint.com/discrete_mathematics/discrete_mathematics_propositional_logic.htm

1. a collection of declarative statements which have a truth value of either
   `true` or `false`
1. a set of "connectives" (binary functions) which take two statements and
   produce one statement
    - connectives are **defined** by their truth tables
    - some connectives:
        1. AND
        1. OR
        1. NOT
        1. XOR
        1. Implication or _if then_
        1. If and only if

AND, OR, NOT work as expected

Aside: IIRC there is a theorm which says you can represent any of the other
connectives in terms of AND and NOT

## Implication

A -> B

- is false if A is true and B is false, otherwise is true
- this is quite unintuitive to me

```
A       B       A->B
true    true    true
true    false   false
false   true    true
false   false   true
```

```
Consider a seller making the statement

    If the banana are yellow then the banana is ripe

A = banana is yellow
B = banana is ripe

If the banana is yellow and it is ripe then the seller told us the truth
If the banana is yellow and it is not ripe then the seller lied
If the banana is green the the sellers statement does not apply BUT it also means he did not tell us a lie so the statement is "not false" i.e. it is true
A->B is linking the truthiness of B to the truthiness of A - it is a meaningless statement if A is false but that does not make it false

A       B       A->B
true    true    true
true    false   false
false   true    true
false   false   true

```

> http://www.cs.odu.edu/~cs381/cs381content/logic/prop_logic/connectives/note.html
>
> ( P -> Q ) is True whenever P is False as well as when both P and Q are true
> according to the Meaning of Connectives.
>
> This might be counterintuitive for some people and might be a little difficult
> to be convinced of.
>
> What we are concerned about here is True or False of the statement ( P -> Q ).
> You might also look at it this way. We are interested in whether or not the
> person who made this satement is lying. If the statement is False, then that
> person is lying. For example consider this sentence:
>
> You get ten thousand dollars from me if I win one million dollars in a
> lottery. Here P is "I win one million dollars in a lottery" and Q is "You get
> ten thousand dollars from me".
>
> If I don't win the lottery (P is False), I don't have to give you ten thousand
> dollars (Q is False). My statement ( P -> Q ) is still true if you don't get
> the money from me when I don't win. I haven't lied to you.
>
> This is what "( P -> Q ) is True when P is False" means.
>
> Similarly for when P and Q are False.
>
> On the other hand, if I did win the the lottery and did not give you $10,000,
> then I have lied to you, that is the statement "You get ten thousand dollars
> from me if I win one million dollars in a lottery" is not true. That is what
> "( P -> Q ) is False if P is True and Q is False" means

## If and only if

- the output is true if the inputs A and B have the _same_ truth value i.e.
    - output is true if A and B are both true
    - output is true if A and B are both false
- is the opposite of XOR

## Tautologies

- A forumla which is always true for for all possible values of its input
  statemetns e.g.

    [(A→B)∧A]→B

## Contradiction

- A forumla which is always false for all possible values of its input
  statements

    (A∨B)∧[(¬A)∧(¬B)]
