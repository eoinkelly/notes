# Rich Hickey Talk

Design is: make a plan, write it down

> design is about taking things apart in such a way that they can be put back
> together

- take things apart and THEN compose them together to build the thing you need
- each compontent should be "about" one or a very few things
- design is _iterative_

Consider taking things apart ...

- Taking apart requirements
    - move from statements of want/need to the underlying _problems_
    - move from knowns to unknowns
    - move from domain-side to solution-side
        - domain side: we might need domain expert to tell us how to solve it
        - solution side: where will we run it, how will it scale etc.
    - differentiate causes and symptoms
    - some requirements are unstated
        - because they are future problems the client doesn't want to have
        - because they think they are obvious and don't need to be stated e.g.
            - "it should not blow up"
            - "it should not run very slowly"
            - "it should not run out of memory"

- we can take apart time/order/flow with techniques such as
    - queues
    - idempotency
    - commutation
    - transactions

- Take apart place, participants
    - does everybody have to use same tool?
    - can i do it with a huge team?
    - does team have to be in same place?

- differntiate between _Information_ vs mechanism
    - info: the set of logged in users
    - mechanism: the ruby Set class
    - we only have programming language constructs to represent both of these
      things. Language constructs are not suitable for representing
      _information_
        - I think he is talking about immutability here

- Take apart solutions
    - benefits
        - what is good
    - trade-offs
        - what is not good
        - i did X but that means I can't do Y
    - costs
    - problem fit
        - is there another solution that is a _closer_ fit for the problem

Design helps us to

1. understand the system
2. coordinate between teams
3. extend the system the design has connection points that allows it to connect
   to other things
    - design is not just about accreting up stuff until you have an answer
4. reuse the system
    - reuse comes from design _not_ programming language constructs
5. testing
6. efficiency
    - it is easier to iterate a design rather than iterate a solution

In the arts, "composers" have a blank canvas and they add constraints so they
create a "design problem"

### specificity and scale

- in music
    - fully orchestrated/arranged pieces are more typical at larger scales e.g.
      orchestra pieces
    - just "melody and changes" pieces are more common at smaller scales
        - gives more lassitude for performers but they also have increased
          responsibility
- it seems to be the oppisite in software

### On constraints

- most compositions are "about" one or a few things
    - melodic, harmonic, rhythmic, timbral ideas
    - they have a single motif or theme
- larger works are more stratified
    - each layer is "about" a different thing but each layer is still about 1 or
      a small number of things

- in the performance space, only the melody and chord changes are given
- performer provides variations
- improv needs a lot of preparation, practice, study e.g. John Coltrane
  practiced a lot!
- you hear in the performance the application of a lot of knowledge and
  vocabulary

- harmonic sensibility is a key design skill
    - melody is about sequentiality
    - harmony is about simultaneouty
    - a harmonious app is one where each of the layers works well together
        - APIs are not overlapping
- RH thinks programming languages are like instruments
    - he compares a single key blues harmonica with a DSL
- You may want to play a "choose-a-phone" but nobody wants to _compose for_ a
  "choose-a-phone" (a crazy instrument you built yorself)
- instruments are made for people who can play them
    - shouldn't cellos autotune?
    - maybe they shouldn't make any sound until you get it right?
- we shouldn't target beginners in our designs nor should we elimiate all effort
- is pairing just a way of us trying to "buy some design time"
- musicians spend way more time practicing than performing
- programmers seem to think we can just show up every day

> In order to be creative you have to prepare to be creative

RH thinks what is wrong with SQL is that we only have a human interface for it
not also a machine interface.

If you build a machine interface you can build the human interface on top of it

- he thinks that because _all_ our layers are code we are too distracted by
  building the instrument rather than practiciting our playing
    - we are all "luthiers" (people who build guitars)
    - leads to distraction - if I knew how to build guitars I'd spend a lot less
      time playing guitar
- "just use emacs" is like somebody wanting to make music and we hand them a
  soldering iron
- we are obsessed with choices - remember _constraint is a driver of
  creativity!_
- RH says: Quit fidgeting
