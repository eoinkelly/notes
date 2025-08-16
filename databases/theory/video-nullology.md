# Nullology

Source

- https://learning.oreilly.com/videos/nullology/9781491908822/9781491908822-video182157

- Nullology is the study of the _empty set_
- Empty set is the analog of zero in arithmetic
- Empty set is a very useful edge-case when testing theorems etc.

## Topics in the video

- Empty relation bodies aka "empty relations"
- Empty relation headings
- Empty sets of operaands
- Empty subsets of headings
- Empty LHSs in FDs and empty keys
- Empty types

Aside: SQL fails in all these areas

relvar = short of "relation variable"

## Basic operators in relational algebra

RENAME

    S RENAME (CITY AS SCITY)

Restriction

    S WHERE STATUS < 25

Projection

    S {SNO, CTY} # S(ALL BUT SNAME, STATUS)

Product

    (S RENAME {CITY AS SCITY}) TIMES (P RENAME {CITY AS PCITY})

Union/intersection/difference

    S {CITY} UNION P {CITY}
    S {CITY} INTERSECT P {CITY}
    S {CITY} MINUS P {CITY}

Join S JOIN SP S JOIN P # must have one attribute with the same (name, type) in
both relations

Extend

    # create a new attribute dynamically in relation P
    EXTEND P: {GMWT = WEIGHT * 454}

Summarize

    SUMMARIZE SP BY {PNO}: {TOTQ := SUM(QTY)}

### Consider how these operations work on the empty set

let `mt` be the empty relation

    mt RENAME ... = an empty relation
    mt WHERE = mt
    mt {...} = an empty relation
    mt INTERSECT r = mt
    mt MINUS r = mt
    mt UNION r = r
    r TIMES mt = mt TIMES r = an empty relation (not the same one as mt because heading different)
    r JOIN mt = mt JOIN r = an empty relation (not the same one as mt because heading different)
    EXTEND mt = an empty relation
    SUMMARIZE mt BY {...} = an empty relation

    r TIME
