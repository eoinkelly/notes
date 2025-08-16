# Queueing Theory

Kendall's notation to describe a queue

general = arbitrary

    <interarrival time distribution>/<service time distribution>/<number of servers>
    G/G/1 = <general interarrival time distirbtion>/<a diffeerent but also general service time distribution>/<one server>

A G/G/1 queue is kind of a worst case scenario for us knowing metrics about the
quueo

## Kingsman's formula to approximate the mean waiting time of a G/G/1 queue:

$$E(W_q) \approx \left( \frac{\rho}{1 - \rho}\right) \left( \frac{c_a^2 + c_s^2}{2} \right) \tau$$

Terms

- $\rho$ = Utilisation
- $c_a$ = the coefficient of variation of arrival times
    - the standard dev of arrival timed divided by the mean arrival time
- $c_s$ = the coefficient of variation of service times
    - the standard dev of service timed divided by the mean service time
- $\tau$ = mean service time
- $E(W_q)$ = the mean waiting time in the queue

Notes

- this equation is **an approximation**
- Note that mean/avg wait time increases with the square of the variability in
  arrival times and service times
    - i.e. small changes in those values have large impact

Things which impact the waiting time in a queue

1. Processing Time of each job
1. Utilization
1. Variation of
    1. processing times
        - how much variation in processing time is there
    2. Arrival variation
        - if for ex. more than usual jobs arrive in a short period it may take
          longer than expected for the process to recover and catch up

Utilisation

    what you really do / what you can do

A round first attempt at kingsman equation:

    waiting time = job time x effect of utilisation x effects of variations
