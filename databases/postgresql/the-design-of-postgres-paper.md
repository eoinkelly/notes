# Academic paper: The design of postgres

It has the idea of columns of type POSTQUEL which contain queries When you read
a POSTQUEL column the queries in it are run and return data from other tables
and parts of the DB this is kind of a "dynamic column" I guess

Does anything implement this idea these days? Is this what triggers evolved
from?

New operators must tell the query optimizer

1. how fast their various paths are
1. what join strategies are feasible
    - nested iteration is always feasible
    - but merge-join and hash-join may not always be

Postgres proposes using an _I lock_ - TODO: find out more

TODO:

> All relations will be stored (me: on disk) as heaps (as in [ASTR76])
