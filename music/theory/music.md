# Music

-   [Music](#music)
    -   [Overview](#overview)
    -   [Tuning](#tuning)
        -   [Why the 7 letters for notes?](#why-the-7-letters-for-notes)
        -   [Why is C the "starting point"?](#why-is-c-the-starting-point)
        -   [Alternatives to 12 notes/octave](#alternatives-to-12-notesoctave)
        -   [Cents](#cents)
        -   [Temperament](#temperament)
        -   [12 tone equal temperament (12ET)](#12-tone-equal-temperament-12et)
    -   [Intervals](#intervals)
        -   [Interval names and scale names use "major","minor" differently](#interval-names-and-scale-names-use-majorminor-differently)
    -   [Scale degrees](#scale-degrees)
        -   [Intervals vs scale degrees](#intervals-vs-scale-degrees)
        -   [Interval names within a major scale (true for any major scale)](#interval-names-within-a-major-scale-true-for-any-major-scale)
    -   [Scales](#scales)
        -   [Chromatic scale](#chromatic-scale)
        -   [Major scale](#major-scale)
        -   [Minor scales (3 related patterns)](#minor-scales-3-related-patterns)
            -   [Natural minor](#natural-minor)
            -   [Harmonic minor](#harmonic-minor)
            -   [Melodic minor](#melodic-minor)
        -   [Pentatonic scale](#pentatonic-scale)
        -   [Blues scale](#blues-scale)
        -   [Aside: Types of scale](#aside-types-of-scale)
    -   [Modes](#modes)
        -   [7 essential modes](#7-essential-modes)
    -   [Keys (annotating written music)](#keys-annotating-written-music)
        -   [Key signatures](#key-signatures)
            -   [Reading key signatures](#reading-key-signatures)
        -   [Blue notes vs Accidentals](#blue-notes-vs-accidentals)
    -   [Chords \& harmony](#chords--harmony)
        -   [Tertian harmony](#tertian-harmony)
        -   [Major scale harmonies(chords)](#major-scale-harmonieschords)
            -   [Cmaj scale example](#cmaj-scale-example)
        -   [Mapping major scale degrees to intervals](#mapping-major-scale-degrees-to-intervals)
        -   [Natural minor chords](#natural-minor-chords)
    -   [More details](#more-details)

## Overview

![How notes map to frequencies](./notes-to-frequencies.png)

Original:
https://docs.google.com/spreadsheets/d/1jeIa2PHvUnrhHvyD5D9zVmV7eO89LbLETm-np8UuNwY/edit#gid=168383079

There are 4 main aspects of music:

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
        - example:
            - Guitar open low E -> F is ~10 Hz
            - Guitar high E string 12th fret E -> F is ~78 Hz
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

## Tuning

A = 440hz

-   11 named pitches
    -   7 with one name
    -   4 with two possible names (flat or sharp) (these are called _enharmonic_)
-   12 pitches in an octave
    -   so the root must be shared between the octave above and below it
-   it is not the absolute frequencies that matter - it is the intervals between the frequencies
    (the interval distance in Hz gets larger as notes get higher
-   a melody is a sequence of **intervals**
-   the names of the intervals are subjective & based on how they feel to us

### Why the 7 letters for notes?

Using the 7 letters mean the same 7 **letters** are used in every key - this is supposedly easier to
understand than having to memorize which letters are skipped.

The "no repeat letters" thing applies most of the time but not always:

-   it's impossible to write the blues scale without double a note given three notes with two half
    step gaps.
-   also octatonic scales (TODO: what are these?)

Ancient greeks noticed:

-   Doubling the length of a string gives a very harmonious outcome
-   Tripling the length of a string gives a harmonious outcome
-   Quadrupling the length of a string gives a very harmonious outcome

They thought in terms of string lengths, we know this maps directly to frequencies now. They
experimented with different length strings to produce different outcomes

They noticed that the ratios of 3/2 and 4/3 made for nice sounding output This later solidified into
the major triad.

Together 3/2 and 4/3 divide up the octave

### Why is C the "starting point"?

> Notes do not "start" with C; C major is just the easiest major key to notate in modern notation.
> The concept of a major key came about long after letters were assigned to the notes. Before there
> were major (and minor) keys, people used modes, usually just using the notes of the modern white
> keys and starting and ending in different places. The Ionian mode (which became modern major) was
> a late addition to the modes.
>
> So it's historical accident that C major is treated as "basic."

### Alternatives to 12 notes/octave

There are alternatives to 12 notes in octave:

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

### Cents

-   A unit of measure for the **ratio** between two frequencies
-   An equally tempered semitone is 100 cents **by definition**
-   An octave is 1200 cents by definition

### Temperament

![Western tuning comparisons](./western-tuning-comparisons.png)

-   "Just intonation" tunes the instrument based on ratios - it is the most mathematically pure way
    to do it
-   if you use ratios to tune your instrument it works fine for one octave root but the ratios do
    not hold if you use one of your other intervals as the root
-   => your music will sound good with one root but not in others
-   => you cannot move your composition around
-   you would have to re-tune your instrument to play in a different key

We fix this with "temperament" - we have to "temper" the intervals to make the tuning more
versatile. Over history various "temperaments" have been used:

1.  Pythagorean
2.  Meantone
3.  Well temperament
4.  Equal temperament

Tempering is a compromise between keeping the intervals within an octave sounding good and being
able to move around

The (almost) universally adopted modern system is _12 tone equal temperament_.

### 12 tone equal temperament (12ET)

-   12ET makes every interval equidistant.
-   Steps
    1. tune the octave with the perfect mathematical ratio
    2. divide the octave into 12 equal intervals
-   12ET is technical slightly out of tune but we are so used to it that we don't really notice.
    -   Tempering is a compromise between keeping the intervals within an octave sounding good and
        being

| Interval name | Just Int. ratio | 12TET adjustment (cent) |
| ------------- | --------------- | ----------------------- |
| m2            | ?               | +11.73                  |
| M2            | ?               | +3.91                   |
| m3            | 6:5             | +15.64                  |
| M3            | 5:4             | -13.69                  |
| P4            | 4:3             | -1.96                   |
| T             | ?               | -17.49                  |
| P5            | 3:2             | +1.96                   |
| M6            | 8:5             | -15.64                  |
| m6            | 5:3             | -13.69                  |
| M7            | ?               | -11.73                  |
| m7            | ?               | +17.6                   |
| Octave        | 2:1             | 0                       |

## Intervals

So it's all about **frequency intervals** and not much to do with absolute frequencies

-   The octave is the most consonant interval and therefore the most important interval
-   the interval of an octave is the most consonant (we perceive them as so close that they are the
    deeper or higher versions of the "same note") so we give them the same name
-   so the system starts by finding octave intervals
-   the math relationship between octaves are also clean - the frequencies double/halve

So in our imaginary "building music from scratch" we have the octave interval.

-   now we need to divide the octave up into smaller intervals based on how it sounds to our ears
-   these divisions have names based on their distance from the octave frequencies
-   all these divisions are defined as jumps from the "base" octave frequency
-   the more consonant an interval sounds to human ears, the simpler the relationship between the
    two notes

Notation

    M = major
    m = minor

| Interval | Just Intonation mult. | Just Intonation | To the ear       | 12TET adjustment (cent) |
| -------- | --------------------- | --------------- | ---------------- | ----------------------- |
| Octave   | 2                     | 2:1             | Most Consonant   | 0                       |
| P5       | 1.5                   | 3:2             | Highly Consonant | +1.96                   |
| P4       | 1.33                  | 4:3             | Highly Consonant | -1.96                   |
| M3       | 1.25                  | 5:4             | Consonant        | -13.69                  |
| m3       | 1.2                   | 6:5             | Consonant        | +15.64                  |
| M6       | 1.6                   | 8:5             | Consonant        | -15.64                  |
| m6       | 1.66                  | 5:3             | Consonant        | -13.69                  |
| M2       | ?                     | ?               | Dissonant        | +3.91                   |
| m2       | ?                     | ?               | Dissonant        | +11.73                  |
| M7       | ?                     | ?               | Dissonant        | -11.73                  |
| m7       | ?                     | ?               | Dissonant        | +17.6                   |
| T        | ?                     | ?               | Dissonant        | -17.49                  |

Remember we are just naming **intervals** here, not naming notes!

We want the intervals within the octave to be _more or less_ evenly spaced because it makes it much
easier to move music up/down the frequency range (i.e. change key)

Layout of our interval names within an octave

```
root m2 M2 m3 M3 P4 Tritone P5 m6 M6 m7 M7 8ve
```

Aside: Power chord = root + P5 or root + P5 + 8ve

How human ears perceive intervals, from most consonant to most dissonant:

```
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
```

### Interval names and scale names use "major","minor" differently

NOTE: The "major" and "minor" in the interval names only sometimes correspond to major and minor
scales - it's just a coincidence when they do. There are no _"major scales use the major intervals"_
or _"minor scales use the minor intervals"_ rules. In interval naming, major and minor can be
thought of as "big" and "little"

Major, minor in intervals refers to the distance (in semitones) between notes.

Major, minor in scale names reflects an overall vibe of the scale The scales were names after the
vibe of the 3rd **scale degree** in the scale. If 3rd scale degree = M3 then it's a major scale If
3rd scale degree = m3 then it's a minor scale

## Scale degrees

-   A way of referring to the notes in a scale by their distance from the first and main note of the
    scale
-   A "scale relative" way to refer to notes within a scale
-   A short-hand for naming notes within a scale
    -   Rather than specifying each note, you specify the pair (scale-root, scale-degree)
    -   It tells you about the role of that note rather than just the note e.g. compare "play a D"
        to "play a 2/supertonic/II in C major" - the latter gives you way more usable info.
    -   The role of 2/supertonic/II is the same in any scale so it makes sense for musicians to
        refer to the note by it's job
    -   The conversation still makes sense even if you transpose keys.
-   The distance in semitones between each degree varies and is what gives the scale it's character
-   scale step = distance between two scale adjacent scale degrees.
    -   Maps to variable number of semitones.
-   whole step, half step = distance between intervals
    -   (fixed at whole = 2 semitones, half = 1 semitone)
-   Two names are _enharmonic_ if they are aliases for the same note.
-   Every musical scale has 7 _degrees_. scale.

Scale degrees are named in multiple ways:

1. Numerals: 1-7
2. Roman numerals: I - VII
3. English names
    1. Tonic
    2. Supertonic
    3. Mediant
    4. Subdominant
    5. Dominant
    6. Submediant
    7. Subtonic **or** Leading note
        - called Subtonic when it is a 2 semitones below tonic
        - called Leading note when it is 1 semitone below tonic
4. _movable do solfége_ system: do, re, me, fa, so, la, ti

### Intervals vs scale degrees

-   Intervals map to a fixed number of semitones
-   Scale degrees map to different number of semitones in different keys

### Interval names within a major scale (true for any major scale)

-   The major scale has a fixed mapping between intervals and scale degrees. Given by
    ```
    Degree:       1 2 3 4 5 6 7
    Interval gap: W W H W W W H
    Interval gap: 2 2 1 2 2 2 1
    ```
-   The primary names below are derived from the major scale
-   The natural state of the intervals is the major scale are the "major intervals".

| # semitones | Deg. num | Deg. name    | Primary Name(s) | Aliases                                 |
| ----------- | -------- | ------------ | --------------- | --------------------------------------- |
| 0           | 1        | Tonic        | Perfect unison  | Diminished 2nd                          |
| 1           |          |              | Minor 2nd       | Flat 2nd                                |
| 2           | 2        | Supertonic   | Major 2nd       | Natural 2nd                             |
| 3           |          |              | Minor 3rd       | Augmented 2nd, Flat 3rd                 |
| 4           | 3        | Mediant      | Major 3rd       | Diminished 4th, Natural 3rd             |
| 5           | 4        | Subdominant  | Perfect 4th     | Augmented 3rd                           |
| 6           |          |              | Tritone         | Augmented 4th, Diminished 5th, Flat 5th |
| 7           | 5        | Dominant     | Perfect 5th     | Diminished sixth                        |
| 8           |          |              | Minor sixth     | Augmented 5th, Flat 6th                 |
| 9           | 6        | Submediant   | Major sixth     | Diminished 7th, Natural 6th             |
| 10          |          |              | Minor 7th       | Augmented sixth, Flat 7th               |
| 11          | 7        | Leading note | Major 7th       | Diminished octave, Raised 7th           |
| 12          | 8        | Tonic        | Octave          | Augmented 7th                           |

-   Alternative names for intervals
    -   all minor intervals also known as "flat" or "flattened" intervals
    -   major intervals can be called "natural" intervals
    -   major 7th can be called "raised 7th"
    -   minor 6 also called augmented 5th
    -   Augmented and diminished intervals
        -   NB: notes are not augmented or diminished - only **intervals** are augmented or
            diminished.
        -   The rules around augmented and diminished intervals are:
            -   When you raise a **perfect** interval by a half-step it is called augmented
            -   When you lower a **perfect** interval by a half-step it is called diminished
            -   When you raise a **major** interval by a half-step it is called augmented
            -   When you lower a **minor** interval by a half-step it is called diminished
            -   you cannot create an augmented interval by raising a minor interval
            -   Note that you cannot create an diminished interval by lowering a major interval

## Scales

What is a scale?

-   A scale is 8 successive pitches within a one octave range
-   Always ends on the same note as the first note but one octave higher
-   Each scale has 8 _degrees_
-   Different types of scale have different distances between their degrees. These different
    intervals are what makes the scales different
-   If you play notes which are part of the scale they are _diatonic_. If you play notes outside the
    scale they are _chromatic_ (chroma = color => chromatic notes add color to a piece)
-   There is one major scale and multiple minor scales for each note
-   Major is written with an 'M' and minor is written with an 'm'
-   A scale is defined by
    1. its first degree/tonic note
    2. it's interval pattern
-   Based on their interval patterns, scales are grouped into categories such as major, minor,
    chromatic etc.
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

### Chromatic scale

A _chromatic_ scale includes all the notes within one octave (including sharps and flats)

    1 1 1 1 1 1 1 1 1 1 1
    half half half half half half half half half half half

### Major scale

A Major scale is defined by it's intervals:

    W W H W W W H
    2 2 1 2 2 2 1
    2 2 1    2    2 2 1 (Ancient greek view: 2x tetrachords joined by a whole step interval, called Ionian mode)
    whole whole half whole whole whole half

On a piano, the C Major scale _happens_ to be just white keys but **all other major scales have
black keys in them** i.e major scales are not "white key scales", it just happens to be the case on
that one instrument (Piano) for that one Major scale (C Major)

Scales can be enharmonic too e.g. C# Major is enharmonic to Db Major

There is a major scale starting on each of the 12 pitches which means there are 12 major scales. 4
of the pitches have two names (# and b) so there are 16 ways to write the 12 scales.

There is only one major scale for each pitch.

### Minor scales (3 related patterns)

There is only one kind of major scale but many kinds of minor scale.

There are many minor scales but 3 are common:

1. Natural minor
2. Harmonic minor (also called Enharmonic)
3. Melodic minor

So you could say there are 48 commonly used scales in music.

Scales are named after their most significant intervals, which happens to be the 3rd (in both the
major and minor scales). (Major and minor chords, likewise, are named after their 3rds.)

The "major scale" just happens to have the bigger kind of 2nd 6th and 7th as well. But there are
other "major modes", like mixolydian and lydian.

Likewise there are plenty of "minor" scales and modes, all of which share the minor (smaller) 3rd,
but the 2nd 6th and 7th can all vary in size.

#### Natural minor

```
2 1 2 2 1 2 2
whole half whole whole half whole whole

root 2 1 2 2 1 2 2
root whole half whole whole half whole whole
C     D     Eb    F     G     Ab    Bb     C
```

-   Is just a major scale shifted
    -   starts on the 6th degree of it's relative/corresponding major scale
    -   up 9 half-steps or down 3 half-steps
    -   up 5 degrees or down 2 degrees
-   has exactly the same 8 notes as it's corresponding Major scale
-   The notes in natural minor is what is used for the minor key signatures (not harmonic or melodic
    minors)

#### Harmonic minor

-   A variation on the natural minor
-   Natural minor but the 7th is raised by one semitone
-   Nice to play, awkward to sing. Hence the melodic minor
-   is a common foundation for chords (harmonies) in minor keys

Naming:

-   1 = half
-   2 = whole
-   3 = aug-2nd

```
root  2 1 2 2 1 3 1
root whole half whole whole half  aug-2nd half
C    D     Eb   F     G     Ab    B       C
```

#### Melodic minor

```
root 2 1 2 2 2 2 1
root whole half  whole whole whole half
C    D     Eb    F     G     A     B     C
```

-   Some (but not all) music theorists use melodic minor when going up a scale but use the natural
    minor notes when going down.

### Pentatonic scale

-   Major and minor scales can also be called heptanoic (7 note per octave) scales but it isn't
    common
-   2 types of pentatonic (5 note per octave) scales
    1. hemitonic = pentatonic which includes semitones
    2. anhemitonic = pentatonic which doesn't include semitones
-   What do the types mean?

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
          A minor pentatonic starting on the 6th just like Cmaj has C natural minor starting on the
          6th
    - E A D G B E (Guitar open strings) are E minor pentatonic

### Blues scale

TODO: I know nothing here

### Aside: Types of scale

| Name       | Notes per octave |
| ---------- | ---------------- |
| Chromatic  | 12               |
| Nonatonic  | 9                |
| Octatonic  | 8                |
| Heptatonic | 7                |
| Hexatonic  | 6                |
| Pentatonic | 5                |
| Tetratonic | 4                |
| Tritonic   | 3                |
| Ditonic    | 2                |

:question: why no 10 and 11?

:question: Wikipedia describes scales as "being explored" in 19th and 20th century. I guess you just
find a combo of notes that sounds good to you and other people and then you have a scale?

## Modes

Q: are they combinations of notes not represented by the combo of major + minors + modes?

-   Once you get past the major and minor scales, all the other 8 note combinations are called
    **modes** instead of scales.
-   we think of modes are related to a major scale but in practice they are their own set of
    intervals
-   modes give a different twist on the major scale - you get a different feel while staying within
    the same notes of the traditional major scale
-   Modes pre-date scales by a lot (modes date to ancient greece):
    -   Major scale is based on the Ionian mode.
    -   Minor scale based on the Aeolian mode
-   The number and use of modes expanded in the era of the medieval church - these were called
    _church modes_

### 7 essential modes

-   The 7 essential modes are rotations of the major scale intervals
-   each one starts on a different degree of the major scale but uses the same notes as it's major
    scale

| Mode name  | Starting degree | Intervals     | Notes                                                  |
| ---------- | --------------- | ------------- | ------------------------------------------------------ |
| Ionian     | 1               | 2 2 1 2 2 2 1 | **exactly** the major scale                            |
| Dorian     | 2               | 2 1 2 2 2 1 2 | pretty close to natural minor                          |
| Phrygian   | 3               | 1 2 2 2 1 2 2 | pretty close to natural minor                          |
| Lydian     | 4               | 2 2 2 1 2 2 1 | major ish sounding                                     |
| Mixolydian | 5               | 2 2 1 2 2 1 2 | also major ish sounding                                |
| Aeolian    | 6               | 2 1 2 2 2 1 2 | **exactly** the natural minor scale                    |
| Locrian    | 7               | 1 2 2 2 1 2 2 | sounds unusual, unused for centuries, used in Jazz now |

## Keys (annotating written music)

-   A key = a scale
-   Writing the sharp/flat symbol for every note would be annoying to write and to read
-   So the idea of a "key" was invented to set a default in terms of what should be sharp and flat
    for a piece
-   Each key corresponds to a scale.
-   When you assign a key to apiece then people assume most of the notes will stay within that scale
    and you only have to annotate the exceptions.
-   The exceptions are called "accidentals", a name which makes more sense in this context than in
    the "general name for sharps and flats" context

### Key signatures

-   You specify a key by setting a "key signature" at the very start of a piece.
-   The key signature specifies which sharps and flats to use for the whole piece
-   There is a key for each scale
-   There are 15 major scales (including 3 enharmonics) so there are 15 major keys
-   Natural minor scales are rotations of the major scales so they share the same key signature as
    their corresponding major scale because they include the same notes

#### Reading key signatures

-   Single flat => Key of F
-   Multiple flats => Look at the next-to-last flat to get the key
-   look at last sharp, next note up is the key
    -   last sharp in th key signature represents the 7th degree of that particular scale so the
        tonic of the scale is the next note up

### Blue notes vs Accidentals

-   Notes outside the current key (scale) are called "accidentals"
-   Bends make "blue notes", notes which are not exactly on a semitone, come from jazz and blues

## Chords & harmony

### Tertian harmony

-   Tertian harmony is the most commonly used - is built from 3rds (in scale degrees).
-   Or you can think of it as being built by skipping one scale degree before adding a note to the
    chord
-   Sometimes called 1-3-5 chords (where the numbers are _scale degrees_)

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

These chords fall out of following the 1-3-5 pattern for each note.

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
deg 5 = F (+6 semitones from root = tritone = diminished fifth = flat fifth)
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

### Natural minor chords

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

## More details

-   https://www.youtube.com/watch?v=lvmzgVtZtUQ
