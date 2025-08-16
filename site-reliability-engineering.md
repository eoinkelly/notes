# Site reliability engineering book

> reliability is “The probability that [a system] will perform a required
> function without failure under stated conditions for a stated period of time”

> Software engineering has this in common with having children: the labor before
> the birth is painful and difficult, but the labor after the birth is where you
> actually spend most of your effort.

- most literature focues on the first 9 months (so to speak)
- software does not "stabilize" in production instead software objects have a
  lifecycle
- lifecycle of software objects
    - inception
    - development
    - deployment in production
    - refinement
    - decommission
- engineers that oversee that whole cycle (not just the development bit) are
  called SREs at Google
- SRE Site Reliability Engineer
    - SRE is a job title within Google
    - design and develop software that works with the products
        - sometimes it is the infrastructure that the products sit on top of
        - sometimes it is assembling the pieces the products need e.g. backups,
          load balancing (ideally in a reusable way)
    - find ways to make systems
        1. more reliable
            - reliability is so important it is part of the role's title
            - their focus differs a bit from devops:
                > distinct from the industry term DevOps, because although we
                > definitely regard infrastructure as code, we have reliability
                > as our main focus. Additionally, we are strongly oriented
                > toward removing the necessity for operations
            - SREs try to remove the necessity for operations!
            - they stop once a system is "reliable enough"
        2. more scalable
        3. more efficent
    - are about keeping _services_ running, not _sites_ (despite the name)
    - think in the "SRE way"

> managing change itself is so tightly coupled with failures of all kinds

Book sections _ background on google services to allow understanding of the
other points _ description of general practices \* google specific practives

Authors believe that even small organisations should put "lightweight
reliability support" in their product early because it is cheaper to expand an
existing structure rather than add a new one.

In all software orgs there is somebody doing SRE work already. A useful way to
get started in an organisation is to find them and formally recognise them,
encourage, reward them

> a thorough understanding of how to operate the systems was not enough to
> prevent human errors

UP TO START CHAP 1

## Chapter 1: Introduction

Comparing the traditional sysadmin approach to the SRE approach

- Sysadmin
    - ++ easy for orgs to undestand
    - ++ easy to hire for (talent pool already available)
    - ++ many examples to emulate
    - ++ many tools (and service companies) available to help/outsource
    - -- direct costs scale linearly with load (manual change management and
      event handling)
    - -- indirect costs
        - the divide in vocabularly, skills and outlook between dev and ops
        - different assumptions about target levels of stability
        - split on incentives, communication, goals, trust
        - can be in conflict over how quickly software can be released to
          production
        - leads to ops team having too many checks and dev team working aroudn
          those checks by having "feature flips" or "incremental updates" that
          skip those checks

SRE team make-u; _ 50-60% google software engineers (hired via their normal
process) _ 40-50% folks who have a 90% of the skillset of the normal google
software engineers but in addition have a set of skills useful for SRE but rare
for software engineers e.g. unix system internals and layers 1-3 networking
expertise

- hire people who will become bored doing tasks manually and have the skills to
  automate them
- it is crucial that SRE teams are focused on engineering not ops
- there is a 50% of time cap on "ops work" for all SREs e.g.
    - tickets, on-call tasks, manual tasks etc.
- the other 50+% of time spent doing development on automating the systems
- they want systems which are _automatic_ not _automated_
- SRE teams need strong management support e.g.
    - they may decide to stop releases for the remainder of the quarter once an
      error budget is depleted
    - need management support for product teams to accept this
    - if ops load goes over 50% it starts overflowing to the product development
      teams
        - tickets assigned to product managers
        - product devs given pages etc.
        - it encourages devs to build systems that don't need manual
          intervention
- target is 1-2 calls per 8-12hr shift
    - gives SREs enough time to fix problem accurately and do postmortems
- google operates under a "blame free postmortem culture"
    - goals are
        - expose faults and apply engineering to fix them \_rather than avoiding
          them or minimizing them!!!
- SREs are responsible for
    - availability
    - latency
    - performance
    - efficiency
    - change management
        - being able to rollback safely if things go wrong
        - quickly and accurately detecting problems
        - progressive rollouts
    - monitoring
        - "monitoring should never require a human to interpret any part of the
          alerting domain"
        - humans should be notified only when they need to take action
            - kinds of notification:
                - TODO
    - emergency response
        - involved in measuring and improving MTTF mean time to failure
        - involved in measuring and improving MTTR Mean time to recover
    - capacity planning

- cap "50% ops time" after which time tasks get farmed out to the product devs

- helps decideon the error budget for the project
    - how much downtime are we ok with? - we can spend that downtime being more
      agressive with new features
- helps run "blame free post-mortem culture"
