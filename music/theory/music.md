# Music

Sources

-   https://www.youtube.com/watch?v=lvmzgVtZtUQ

## Overview

1. Pitch (aka note or tone)
    - the sound itself
    - 12 pitches in western music (within each octave)
    - pitches are organised into scales (a family of notes)
    - scales are the usual birthplaces of melody and harmony
    - melody = the part of music we can sing
    - harmony = two or more pitches at the same time
    - the Hz distance between notes is not constant - the higher the note the bigger the gap to the
      next note
        - seems to be roughly exponential looking
        - see
          https://docs.google.com/spreadsheets/d/1jeIa2PHvUnrhHvyD5D9zVmV7eO89LbLETm-np8UuNwY/edit#gid=168383079
        - ![Hz graph](<CleanShot 2024-02-06 at 12.00.50@2x.png>)
        - Guitar open low E -> F is ~10 Hz
        - Guitar high E string 12th fret E -> is ~78 Hz
2. Rhythm
    - Palpably organised time
    - motion
3. Form
    - most music follows an A-B-A form e.g. verse-chorus-verse, statement-departure-return
4. Tension and release
    - Building and releasing tension is an important facet of a musical piece

Piano Tips

-   C is the white key to the left of two black keys, D is in the middle, E is on the right
-   F is on the left of three black keys cluster, then B, then G and A is on the right

The distance from one interval to another is a called a _semitone_ or _half-step_

## Scales (also known as keys)

-   a bunch of notes arranged in steps into a particular order that gives a sense of "home"
-   the sense of "home" is called tonality
-   tonality is a sort of "musical gravity"
-   scale and key mean the same thing
-   "what key is this in?" == "what scale does this use?"
-   most (but not all) scales are a selection of 7 pitches from the available 12
    -   any note within the 7 is said to be "diatonic" ie within the scale/key
-   the "home note" of the scale is called the tonic/root/do (as in do-re-me...)/one of the scale
-   scales
    -   7 notes per octave = hepatonic
        -   e.g. major and minor scales
    -   5 notes per octave = pentatonic

### Major scales

**All** major scales have the steps form:

    whole-whole-half-whole-whole-whole-half

This form is what makes a major scale

In C major this looks like:

```
     whole whole half whole whole whole half
    C     D     E    F     G     A     B    C
       2     2    1     2     2     2    1
```

There is a major scale starting on each of the 12 pitches which means there are 12 major scales. 4
of the pitches have two names (# and b) so there are 16 ways to write the 12 scales.

There is only one major scale for each pitch.

There are many minor scales but 3 are common:

1. Natural minor
2. Enharmonic minor
3. Melodic minor

So you could say there are 48 commonly used scales in music.

### minor scales

There is only one kind of major scale but many kinds of minor scale.

Scales are named after their most significant intervals, which happens to be the 3rd (in both the
major and minor scales). (Major and minor chords, likewise, are named after their 3rds.)

The "major scale" just happens to have the bigger kind of 2nd 6th and 7th as well. But there are
other "major modes", like mixolydian and lydian.

Likewise there are plenty of "minor" scales and modes, all of which share the minor (smaller) 3rd,
but the 2nd 6th and 7th can all vary in size.

#### Natural minor

The notes in natural minor is what is used for the minor key signatures (not harmonic or melodic
minors)

```
root 2 1 2 2 1 2 2

root whole half whole whole half whole whole

C     D     Eb    F     G     Ab    Bb     C
```

#### Harmonic minor

Natural minor but the 7th is raised by one semitone

Naming:

-   1 = half
-   2 = whole
-   3 = aug-2nd

```
root  2 1 2 2 1 3 1
root whole half whole whole half  aug-2nd half
C    D     Eb   F     G     Ab    B       C
```

#### melodic minor

-   ascending uses pattern below
-   descending uses natural minor

```
root 2 1 2 2 2 2 1
root whole half  whole whole whole half
C    D     Eb    F     G     A     B     C
```

## Tuning

A 440hz

-   11 named pitches
    -   7 with one name
    -   4 with two possible names (flat or sharp)
-   12 pitches in an octave
    -   so the root must be shared between the octave above and below it
-   it is not the absolute frequencies that matter - it is the intervals between the frequencies
    (the interval distance in Hz gets larger as notes get higher
-   a melody is a sequence of **intervals**
-   the intervals are subjective

So it's all about **intervals** and not much to do with absolute frequencies

-   The octave is the most consonant interval and therefore the most important interval
-   the interval of an octave is the most consonant (we perceive them as so close that they are the
    deeper or higher versions of the "same note") so we give them the same name
-   so the system starts by finding octave intervals
-   the math relationship between octaves are also clean - the frequencies double/halve

So we have the octave interval

-   now we need to divide the octave up into smaller intervals based on how it sounds to our ears
-   these divisions have names based on their distance from the octave frequencies
-   all these divisions are defined as jumps from the "base" octave frequency
-   the more consonant an interval sounds to human ears, the simpler the relationship between the
    two notes

Notation

    M = major
    m = minor

| Interval | Just Intonation multiplier | Just Intonation Ratio | To the ear       | 12TET adjustment (cent) |
| -------- | -------------------------- | --------------------- | ---------------- | ----------------------- |
| Octave   | 2                          | 2:1                   | Most Consonant   | 0                       |
| P5       | 1.5                        | 3:2                   | Highly Consonant | +1.96                   |
| P4       | 1.33                       | 4:3                   | Highly Consonant | -1.96                   |
| M3       | 1.25                       | 5:4                   | Consonant        | -13.69                  |
| m3       | 1.2                        | 6:5                   | Consonant        | +15.64                  |
| M6       | 1.6                        | 8:5                   | Consonant        | -15.64                  |
| m6       | 1.66                       | 5:3                   | Consonant        | -13.69                  |
| M2       | ?                          | ?                     | Dissonant        | +3.91                   |
| m2       | ?                          | ?                     | Dissonant        | +11.73                  |
| M7       | ?                          | ?                     | Dissonant        | -11.73                  |
| m7       | ?                          | ?                     | Dissonant        | +17.6                   |
| T        | ?                          | ?                     | Dissonant        | -17.49                  |

![Western tuning comparisons](./western-tuning-comparisons.png)

Remember we are just naming **intervals** here, not naming notes!

We want the intervals within the octave to be _more or less_ evenly spaced because it makes it much
easier to move music up/down the frequency range (i.e. change key)

Layout of our interval names within an octave

    root m2 M2 m3 M3 P4 Tritone P5 m6 M6 m7 M7 8ve

Aside: Power chord = root + P5 or root + P5 + 8ve

How human ears perceive intervals, from most consonant to most dissonant:

    Octave (generally accepted) <most consonant>
    Perfect 5th/Perfect 4th (generally accepted)

    (people disagree with the order from here on)
    Major 3rd and Minor 6th
    Minor 3rd and Major 6th

    (things below here are considered dissonant)
    Major 2nd and Minor 7th
    Minor 2nd and Major 7th
    Tritone
    Microtonal intervals (quarter tone etc.) <most dissonant>

NOTE: The "major" and "minor" in the interval names only sometimes correspond to major and minor
scales - it's a coincidence whent they do. There is no "major scales use the major intervals" or
"minor scales use the minor intervals" rules. In interval naming, major and minor can be thought of
as "big" and "little"

major, minor in intervals refers to the distance (in semitones) between notes major, minor in scales
reflects an overall vibe of the scale The scales were names after the vibe of the 3rd **scale
degree** in the scale. If 3rd scale degree = M3 then it's a major scale If 3rd scale degree = m3
then it's a minor scale

## Aside: A Cent

-   A unit of measure for the **ratio** between two frequencies
-   An equally tempered semitone is 100 cents **by definition**
-   An octave is 1200 cents by definition

## Alternative names for intervals

-   all minor intervals also known as "flat" or "flattened" intervals
-   major intervals can be called "natural" intervals e.g. "natural 3rd"
-   major 7th can be called "raised 7th"
-   diminished 5th also called
    -   flat 5
    -   flattened 5th
    -   augmented 4th
    -   tritone
-   minor 6 also called augmented 5th

## Alternatives to 12 notes in octave

-   examples
    -   24 notes per octave
    -   19 notes per octave
-   -- much more complicated to play
-   ++ moar notes!
-   most people agree that the extra notes are not useful enough for the added complexity
-   12 notes in an octave is the commonly agreed upon compromise
-   some composers have had a play
    -   https://en.wikipedia.org/wiki/Ivan_Wyschnegradsky
        -   https://www.youtube.com/watch?v=tDroa5WTU34
    -   microtonal guitars are a thing
        -   https://www.youtube.com/watch?v=iRsSjh5TTqI

Above we have shown "Just intonation" which tunes the instrument based on ratios

-   if you use ratios to tune your instrument it works fine for one octave root but the ratios do
    not hold if you use one of your other intervals as the root
-   => your music will sound good with one root but not in others
-   => you cannot move your composition around
-   you would have to re-tune your instrument to play in a different key

We fix this with "temperament" - we have to "temper" the intervals to make the tuning more versatile

Over history various "temperaments" have been used:

-   Pythagorean
-   Meantone
-   Well temperament
-   Equal temperament

Tempering is a compromise between keeping the intervals within an octave sounding good and being
able to move around

The (almost) universally adopted modern system is _12 tone equal temperament_

## 12 tone equal temperament

12ET makes every interval equidistant

1. tune the octave with the perfect mathematical ratio
2. divide the octave into 12 equal intervals

12ET is technical slightly out of tune but we are so used to it that we don't really notice.

## FAQ

### Why 7 notes in a scale/key? why not a diff number?

Using the 7 letters mean the same 7 **letters** are used in every key - this is supposedly easier to
understand than having to memorize which letters are skipped

=> In western music, scale/key do not have (for .e.g.) both F and F# ??? is that right ???

Ancient greeks noticed:

-   Doubling the length of a string gives a very harmonious outcome
-   Tripling the length of a string gives a harmonious outcome
-   Quadrupling the length of a string gives a very harmonious outcome

They thought in terms of string lengths, we know this maps directly to frequencies now. They
experimented with different length strings to produce different outcomes

They noticed that the ratios of 3/2 and 4/3 made for nice sounding output This later solidified into
the major triad

Together 3/2 and 4/3 divide up the octave

Why is C the "starting point"

> Notes do not "start" with C; C major is just the easiest major key to notate in modern notation.
> The concept of a major key came about long after letters were assigned to the notes. Before there
> were major (and minor) keys, people used modes, usually just using the notes of the modern white
> keys and starting and ending in different places. The Ionian mode (which became modern major) was
> a late addition to the modes.
>
> So it's historical accident that C major is treated as "basic."

## Pentatonic scale

major and minor scales can also be called heptatonic scales but it isn't common

-   2 types of pentatonic (5 note per octave) scales
    1. hemitonic = pentatonic which includes semitones
    2. anhemitonic = pentatonic which doesn't include semitones

1. Major pentatonic
    - built by removing the 4th and 7th from the major scale
    - you play 1 2 3 5 6 on the major scale
    - `root 2 2 3 2 3`
    - other kinds of pentatonic
        - remove 1st and 3rd of major scale
        - remove 1st and 4th of major scale
2. Minor pentatonic
    - `root 3 2 2 3 2`
    - is related to the major pentatonic similar to how the natural minor is to the major
        - each major pentatonic has a relative minor starting on the 6th e.g. C major pentatonic has
          A minor pentatonic starting on the 6th just like Cmajor has C natural minor starting on
          the 6th
    - E A D G B E (Guitar open strings) are E minor pentatonic

## Harmonising

### Scale degrees

The notes in a scale are counted as "scale degrees" - scale degrees e.g. in C scale, the 3rd scale
degree is E Scale degrees do not have a fixed translation to intervals (semi-tone distances) They
vary between scales.

Tertian harmony is the most common - is built from 3rds (in scale degrees). Or you can think of it
as being built by skipping one scale degree before adding a note to the chord

Sometimes called 1-3-5 chords (where the numbers are _scale degrees_)

```
root
third (+2 scale degree, actual interval varies by scale)
fifth (+2 scale degrees, actual interval varies by scale)
```

You can continue building chords by skipping one scale degree before adding a note e.g.

```
1-3-5
1-3-5-7
1-3-5-7-9
1-3-5-7-9-11
1-3-5-7-9-11-13
etc.
```

### Major scale harmonies(chords)

#### Cmaj scale example

You can (and should) memorise the 1-3-5 chords created by the major scale:

```conf
# major scale
major minor minor major major minor diminished

# Cmaj scale
C     D     E     F     G     A     B
Cmaj  Dmin  Emin  Fmaj  Gmaj  Amin  Bdim
```

### Mapping major scale degrees to intervals

Depending on the first note of the chord, the interval sizes (in semi-tones) will be different e.g.
in Cmaj scale:

```
For root note (C)
deg 1 = C
deg 3 = E (+4 semitones from root = major 3rd)
deg 5 = G (+7 semitones from root = perfect 5th)
=> Cmaj

For 2nd note (D)
deg 1 = D
deg 3 = F (+3 semitones from root = minor 3rd = flat 3rd)
deg 5 = A (+7 semitones from root = perfect 5th)
=> Dmin

For 3nd note (E)
deg 1 = E
deg 3 = G (+3 semitones from root = minor 3rd = flat 3rd)
deg 5 = B (+7 semitones from root = perfect 5th)
=> Emin

For 4th note (F)
deg 1 = F
deg 3 = A (+4 semitones from root = major 3rd)
deg 5 = C (+7 semitones from root = perfect 5th)
=> Fmaj

For 5th note (G)
deg 1 = G
deg 3 = B (+4 semitones from root = major 3rd)
deg 5 = D (+7 semitones from root = perfect 5th)
=> Gmaj

For 6th note (A)
deg 1 = A
deg 3 = C (+3 semitones from root = minor 3rd = flat 3rd)
deg 5 = E (+7 semitones from root = perfect 5th)
=> Amin

For 7th note (B)
deg 1 = B
deg 3 = D (+3 semitones from root = minor 3rd = flat 3rd)
deg 5 = F (+6 semitones from root = tritone)
=> Bdim
```

You can describe the intervals in semitones but using the interval name is more common.

If you go beyond 1-3-5 chords, you can also memorise the chords built from more complex harmonies

```conf
# using 1-3-5-7 chords
maj7 min7 min7 maj7 dom7 min7 min7b5

# using 1-3-5-7-9 chords
maj9 min9 min7b9 maj9 dom9 min9 min7b5(b9)
```

_minor 7 flat 5_ chord also called _half diminished_

You can use the 7,9,11 etc. chord in place of the equivalent basic 1-3-5 to give your playing some
spice.

## Natural minor chords

The chords made from the natural minor are the same as the ones made from it's related major

```conf
# natural minor scale
min dim maj min min maj maj
# or
i, iidim, III, iv, v, VI, VII

# so for Amin scale
A    B    C    D    E    F    G
Amin Bdim Cmaj Dmin Emin Fmaj Gmaj

# Compare this to the related Cmaj scale
C     D     E     F     G     A     B
Cmaj  Dmin  Emin  Fmaj  Gmaj  Amin  Bdim
```
