# Principals of Fast SQL

    This is super alpha and confused but hey it's a start

i tried working from submissions forward but that ended up with a lot of missing
cols

I think you have to work backwards maybe?

Being concerned with perf makes it a lot harder

Maybe we work forwrds and backwards knowing how many rows in each table seems
relevant

my first approach was to created "decorated" versions of the on-disk tables and
then combine them

but I don't know if that's an efficient way to do it?

goals use indexes as much as possible keep the row count as low as possible at
all stages (there will be trade-offs) only select the columns you actually
need - this saves the db emitting them from earlier stages which helps (I
think!) don't make the query hard to understand to hit perf unless there is real
perf need

approaches

    1. work from the tables on disk fowards to your goal table
    2. work from your goal table backwards to the tables on disk

knowing the row count of all tables involved seems like a requirement before
starting testing each bit as we build up the query seems good include testing
row counts

a challenge is that if you optimize for less work in an intermediate step you
might actually end up doing more work overall but it is all pure functional so
maybe a principal is "Try to read everything you need from each table in one go"
its hard to say that's generally good because it would likely differe depending
on how many rows you read each time and it might make the query super hard to
read???
