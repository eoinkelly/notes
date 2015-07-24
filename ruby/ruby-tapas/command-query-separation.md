# CQS

* methods are either commands or queries but not both
* command
    * has side effects
    * we should ignore return value
* query
    * returns a value we care about
    * no side effects ?

It also refers to a full on architectual pattern that uses separate objects for
reading and mutating a resource. Fowler recommends caution re. the pattern.
