# Udacity - Introduction to AI

# Unit 1: Introduction

An AI is referred to as an _intelligent agent_

An agent has an environment

agent can read from environment via sensors
agent can mutate environment via actuators
the "control policy" for the agent is the thing that takes sensor input and makes actuator changes

QUESTION: "co-artificial intelligence"

### Perception-action cycle

The loop of

    sensor-read -> make-decision -> actuator-change

is called the _Perception action cycle_


There are four key attributes of intelligent agents

### 1. Fully observable vs Partial observable

* Fully observable
    * what the agent can sense *any any point in time* is completely sufficient to make the best possible decision
        * => no memory required
    * an environment is _fully observable_ if the sensors can always see the full state of the environment
* Partially observable
    * agent needs memory to make the best possible decision

### 2. Deterministic env or stochastic env

* Deterministic
    * when the agent actuates a change, the outcome of that change on the environment is completely knowable before actuating
    * e.g. you play a piece in checkers
* Stochastic
    * an element of randomness is involved
    * e.g. dice games
    * you can't actuate a change and know exactly what will happen
    * you can't predict what will happen next

### 3. Discrete vs Continious

* Discrete
    * finitely many things you can sense and changes you can actuate
    * e.g. chess
* Continious
    * infinitely many thigns to sense and/or changes you can actuate
    * e.g. darts

### 4. Benign vs Adversarial environments

* Benign
    * e.g. weather: can be random, stochastic but isn't out to get you
* Adversarial
    * e.g. chess: oponent is actually out to get you
    * adversarial envs are harder to make good decisions in

    Checkers
        fully obserable
        deterministc
        discrete
        adversarial

    Poker
        partially observable
        stochasitc
        discrete
        adversarial

    Robotic car
        partially observable
        stochasitc
        continious
        benign

AI is "uncertainty management in computer software"
AI is the disipline you apply when you want to know what to do when you don't know what to do

Reasons for uncertainty

* sensor limits
* adversaries act in unpredictable ways
* stochastic environments
* laziness
* ignorance

# Unit 2: Problem solving

Defining a problem

1. initial state
    * the first state the agent is in
2. action function
    * takes a state and returns a set of possible actions
3. result function
    * given a state and an action it returns a new state
    * models what would happen if the agent took that action
4. goaltest function
    * decides whether a given state is our goal state
    * it only returns true when we get to the goal state
5. path cost function
    * calculates a cost for a given sequence of state to action transitions
    * usually we make the path cost function _additive_ i.e. the cost of the path is the some of the costs of each step along it
6. step cost function
    takes a state, an action and the state that resutls from applying the action to the first state and returns a cost number


```
ACTION(S1) -> {A1, A2,  ... }
RESULT(S1, A) -> S2
GOALTEST(S1) -> TRUE|FALSE
PATHCOST(a sequence of state to action transitions) -> N (where N is the cost of that path)
STEPCOST(S1, A, S2) -> N
```

Up to "example route finding" in section 2

Starting at your initial state
as you follow the possible actions to get to other states
which make up paths

You may be following multiple paths at the same time
The set of states at the tip of each path is called the "frontier"
The set ofstates which are already in one of your paths is called the "explored region"
The set of states which are not part of any path yet is called the "unexplored region"


Now we want to define a function which will take a problem and return a state which matches the goaltest

Tree search superimposes a search tree over the state space
it is a family of funcitons rather than a single algorithm because how we implement `remove_choice`

```
fn tree_search(p: Problem) -> Path {
    // frontier is a set which begins with having only our initial state in it
    let frontier: Set<Path> = Set::new();
    frontier.add(iniitial_state);

    loop {
        if is_empty(frontier) { return FAIL }
        path = remove_choice(frontier); // remove choice can be implemented different ways
        end_state = find_end_state(path)
        if goal_test(end_state) { return path }

        // iterate over all possible actions in the problem
        for a in p.actions(s) {
            extended_path = path.append(result(end_state, a)
            frontier.add(extended_path)
        }
        }
    }
}
```

Tree search will not even notice that is is back-tracking

The "Tree" that tree search builds from the state space branches on what the *possible* paths from a particular node are
Each state in the state-space can appear as multiple nodes in the tree - the tree doesn't care

You can improve tree search so it keeps track of the "explored states" - this prevents it from backtracking
