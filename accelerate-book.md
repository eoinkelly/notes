# Accelerate

## Prefaces

- Authors contend that you don't have to choose between speed and stability -
  good IT practices give you both.
- Their data is based on surveys i.e.
    - it is subjective & perceptual
    - their population sample might not represent the full IT world
- They say a "Westrum generative" organisational culture is important for
  effective software teams
- the book focuses on the bit between commit and running in production (i.e. IT
  delivery), not the full product development pipeline

Things that didn't work for one foreword author

- mandatining agile from the top down
- thinking every team should use the exact same process
- not focusing on measuring
- not measuring the right things
- not focusing on leadership behaviour changing
- treating it like a program rather than creating a new organisational culture

Things which did work

- knowing their cycle time by understanding their value stream map
- starting with a few smaller teams (not boiling the ocean, limit the blast
  radius)
- not having a separate backlog of features, ops work, technical debt work -
  acknowledge that work is work
- use data to drive actions and decisions
- senior leadership support in actions and words (not just words)

> If my team doesn't feel comfortable sharing risks then I will never know
> reality

It is important to demonstrate that in our organisation failure leads to inquiry

## Chapter 1

The study data:

- cross sectional studies
- academic research principles
- 23k responses from 2k organisations across a range of sizes from 10 - 10k
- greenfield and legacy projects
- both agile and waterfall

> The key to a successful change is measuring and understanding the right things
> with a focus on **capabilities** not maturity.

Authors believe a **capabilities** model is the right way to think about this
and that a **maturity** model is not

Maturity models are less useful than capability models here because:

1. Maturity models imply that you "arrive" and can declare yourself done.
   Capability models imply continious improvement
2. Maturity models often perscribe the same set of tools and processes for all
   teams. Teams have their own contexts, their own systems, their own, goals,
   their own constraints - they should decide on what they focus on next to
   achieve a particular transformation - capability models are more dynamic and
   flexible
3. Capability models are **outcome** based whereas maturity models tend to be
   tool or process based. Capability models focus on
    1. outcomes
    2. the key levers which drive those outcomes
4. Maturity models define a static level of process and tooling. They don't
   reflect how fast the industry best performers are changing now.

They identified 24 capabilities in 5 categories which lead to good outcomes

These capabilities are easy to define, measure and improve. This book mostly
covers how to define and measure them and links to a few resources about
improving them.

Categories

1. Continious delivery
    1. Version control
    2. Deployment automation
    3. Continious integration
    4. Trunk based development
    5. Test automation
    6. Test data management
    7. Shift-left on security
    8. Continious delivery
2. Architecture 9. Loosely coupled Architecture 10. Empowered teams
3. Product and process 11. Customer feedback 12. Value stream 13. Working in
   small batches 14. Team experimentation
4. Lean management and monitoring 15. Change approval processes 16.
   Monitoring 17. Proactive notification 18. WIP limits 19. Visualising work
5. Cultural 20. Westrum organisational culture 21. Supporting learning 22.
   Collaboration among teams 23. Job satisfaction 24. Transformational
   leadership

UP TO END CHAP 1

## Chapter 2: Measuring performance

Flaws in previous attempts to measure performance

1. Focused on outputs not outcomes
2. Focused on indivisual or local measures rather than team or global ones

Examples

1. Lines of code
    - devs will game the system if they are measured on it
2. Velocity
    - velocity is designed to be a capactiy planning tool not a performance
      measuring tool
    - cannot reliably compare velocities across teams
    - cannot be reliably used to measure team performance
    - devs will game the system if they are measured on it
3. Utilization
    - more utilization is better only up to a point
    - as utilization approaches 100% lead times (a measure of how fast work can
      be completed) approach infinity
    - very high utilization leads to not capacity for
        - unplanned work
        - changes to the plan
        - improvement work

A successful measure of performance should

1. Focused on outcomes not outputs
    - Don't reward busywork
2. Focused on team and global measures not indivisual or local ones
    - Don't pit teams against each other

They settled on 4 good ways to measure delivery performance:

1. Lead time
    - Part of "Software delivery performance tempo"
    - The time from a customer making a request to the request being satisfied
    - there are 2 parts
        1. The "fuzzy front end" - the time to design and validate the feature
           (harder to measure)
        2. The time to push the implement, test and deliver the feature (easier
           to measure, less variable, mostly what this book cares about)
    - short lead times are also required to fix outages quickly
2. Deployment frequency (as a proxy for batch size which is the important
   measure but is harder to measure)
    - Part of "Software delivery performance tempo"
    - reducing batch size
        - reduces cycle times
        - reduces variablility in flow
        - speeds up feedback cycle
        - reduces risk and overhead
        - increases efficiency
        - increases motivation and urgency
        - reduces costs
        - reduces schedule growth
3. Mean time to restore service (MTTR)
    - Traditionally reliability is measured as time between failures but in
      software failure is inevitable so "time to restore" is more useful
    - How long does it take to restore the primary service you work on?
4. Change fail percentage
    - What percentage of changes to production fail? i.e. need a rollback or
      some other remediation
    - In the context of lean this is the same as "percent complete"

> Astonishingly, these results demonstrate that there is no tradeoff between
> improving performance and achieving higher levels of stability and quality.
> Rather, high performers do better at all of these measures. This is precisely
> what the Agile and Lean movements predict, but much dogma in our industry
> still rests on the false assumption that moving faster means trading off
> against other performance goals, rather than enabling and reinforcing them.

> It’s worth noting that the ability to take an experimental approach to product
> development is highly correlated with the technical practices that contribute
> to continuous delivery.

Very important:

> Before you are ready to take a scientific approach to improving performance
> you must first understand and develop your culture.

END CHAP 2

## Chapter 3

> it is possible to influence and improve culture by implementing DevOps
> practices.

Levels of culture

1. basic assumptions, values and artefacts
    - formed over time as members of a group make sense of activities,
      relationships & events
    - least visible of the levels, things we "just know", hard to articulate
2. collective values and norms
    - more visible to the group, can be discussed and debated
    - values provide a lens through which group members view and interpret the
      relationships around them
    - this is often the "culture" we think about when we think about culture
3. artifacts: mission statements, creeds, technology, formal procedures, heroes,
   rituals

Wurstrum typology of organisational cultures

- Pathological
    - characterized by large amounts of fear and threat
    - people hoard and distort information for political reasons or to make
      themselves look better
- Bureaucratic - rule oriented
    - these organisations protect departments
    - departmetns want to maintain their turf
    - they do this by doing things by the book - their book
- Generative - performance oriented
    - focus on the mission
    - everything is subordinated to good performance

Westrum: organisational culture predicts how information flows through an
organisation.

Westrum: 3 characteristics of good information:

1. it provides answers to the question that the reciver needs answered
2. it it timely
3. it is presented in such a way that it can be effectively used by the receiver

> Good information flow is critical to the safe and effective operation of
> high-tempo and high-consequence environments, including technology
> organizations.

> An additional insight from Westrum was that this definition of organizational
> culture predicts performance outcomes.

> Westrum’s description of a rule-oriented culture is perhaps best thought of as
> one where following the rules is considered more important than achieving the
> mission—

Better information flow (through a generative culture):

- better information is available for making decisions
- decisions which are wrong are more likely to be reversed quicker
- problems are found more quickly

> What they found instead was that “who is on a team matters less than how the
> team members interact, structure their work, and view their contributions”
> (Google 2015).

> accident investigations that stop at “human error” are not just bad but
> dangerous. Human error should, instead, be the start of the investigation.

We can change how people think by first changing how they behave

> “what my . . . experience taught me that was so powerful was that the way to
> change culture is not to first change how people think, but instead to start
> by changing how people behave— what they do” (Shook 2010).

END CHAP 3
