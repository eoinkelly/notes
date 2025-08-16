# Code review

From: https://www.youtube.com/watch?v=PJjmw9TRB7s#t=12

- The goal here is good technical discussion, not "bug finding"
- Code reviews are a a way to have excellent technical discussion
- Review comments should be _conversation starters_

When doing reviews

- give compliments freely
    - compliment when I learned something
- Ask questions, not give commands
- Give the author credit for things they might have though of

"Extract this as a service" vs "What do you think about extracting this as a
service to reduce some duplication?" _ ++ is a question _ explains _why_ it
would be worth doing

"Did you consider ..." "Can you clarify ..."

Anti-patterns

"Why didn't you _just_ ..." words like just, simply, easily pass judgement and
put people on the defensive "Why didn't you do Xis better but is still negative
"Have you considered doing X?" is better

conflict around a change is good

- healthy: we don't agree on the issue
    - there is a minimum bar of quality and after that we are talking about
      trade-offs
    - reasonable people disagree all the time
- we don't agree on the process
    - people ignore feedbac
    - people commit code directly to master

Small PRs are easier to review

What to review

- naming
- single responsibility principle
- complexity
- test coverage
    - as I'm reading, start building expectations about what tests are there
- code review is not QA

A tidy codebase is like a clean kitchen People do view comments on style more
negatively so automated tools can help

At thoughtbot, authors merge their own PRs after a given no. of thumbs-ups
