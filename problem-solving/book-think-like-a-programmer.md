# Book - Think like a programmer

TODO: also look into "How to solve it" by Polya

## Chapter 1

### Talking about "difficulty"

There are many sources of difficulty

1. It is not clear to you what operations you can perform to solve the problem
    * wording may be (deliberately) confusing
    > difficulty in that problem came from overlooking one of the possible operations
    > If you are unaware of all possible actions you could take, you may be unable to solve the problem.
1. The solution may require a longer chain of operations than you can keep in your head
    > difficulty in this problem arises instead from the long chain of operations required by the solution

### 8 Strategies for problem solving

1. Make a plan (this above all else is important)
2. Restate the problem
3. List all the possible actions you can take to move towards a solution
4. Start with the variable which has the most constraints
5. Reduce the problem
6. Look for analogies
7. Experiment
8. Don't get frustrated

Looking at these in more detail

1. Make a plan (this above all else is important)
    * it is vital you have a way to measure progress that isn't just "and then I solved it"
        > Without being able to measure progress, it’s difficult to formulate a strategy.
    * working with a specific goal is always better than random effort even if you don't achieve that goal
        > When solving problems, working with a specific goal in mind is always
        > better than random effort, whether you achieve that specific goal or
        > not.
    * if you run through all the steps in your original plan and you are not
      ready to implement the solution then it is simply time to make another
      plan
    * having a plan is *always* possible
        > having a plan is always possible.
    * but expect your plan to evolve as you move further into the problem
        > Planning also allows you to set intermediate goals and achieve them.
        > Without a plan, you have only one goal: solve the whole problem. Until
        > you have solved the problem, you won’t feel you have accomplished
        > anything. As you have probably experienced, many programs don’t do
        > anything useful until they are close to completion. Therefore, working
        > only toward the primary goal inevitably leads to frustration, as there is
        > no positive reinforcement from your efforts until the end.
        > General Dwight D. Eisenhower was famous for saying, “I have always found that plans are useless, but planning is indispensable.
        > Prussian Helmuth von Moltke, famously said that “no plan survives first contact with the enemy
2. Restate the problem
    * restating the problem in a more formal manner can help to reveal all possible actions
3. List all the possible actions you can take to move towards a solution
4. Start with the variable which has the most constraints
    > you should start with the variable that has the most constraints, or put
    > another way, the variable that has the lowest number of possible values
    * constraints can help eliminate choices
        > constraints may also simplify our thinking about the solution because they eliminate choices.
5. Reduce the problem
    * reduce by add or removing constraints
    * produce an easier problem
    * recognise that there are multiple ways to reduce a problem - maybe list as many as you can think of before doing any
    * solving the reduced problem can help you build up the skill-set to solve the full problem
    > The other lesson is that sometimes problems are divisible in ways that are not immediately obvious.
6. Look for analogies
    * look for a similarity between the current problem and a problem already solved that can be exploited to help solve the current problem
    * recognising analogies the the most important way to improve speed and skill at problem solving
    * but it is the most difficult to develop and takes time to build up the experience
7. Experiment
    * try things and observe the results
    * experimentation is not the same as guessing!
    * in an experiement you have a hypothetisis of what you expect to happen
    * e.g. when playing with a new API make a separate small app or some context to experiement with it outside of your main app
8. Don't get frustrated
    * frustration makes it harder to think clearly
    * Don't take the problem or your struggle with it personally
    * easier said than done.
    * an attitude of detached curiousity is probably the most helpful

UP TO EXERCISES AT END CHAP 1

1. General sudoku strategy
    * see my sudoku solver ruby file in this dir
    * we are looking for squares which have only one possible value
2. Tile variant covered by pictures instead of numbers
    * it definitely increases difficulty
    * my strategy is to find all the tiles in a particular row or colum and get
    those in a train then move the train to get the tiles in place. It is much
    harder to find all the tiles in a particular row/column without numbers
        * it adds a step where I have to discover which tiles belong in a particular row/column
        * options for doing this
            * pick some likely tiles, make them into a train and try them
                * -- likely lots of false starts (trains take many moves to move)
        * it makes me less sure I have a row/column "finished" as I might have made a mistake with the picture
3.
4.
5.
```
for each empty cell
    calculate array of possible values
    if there is only one possible value then fill it in
```
