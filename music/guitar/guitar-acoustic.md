# Acoustic Guitar

## Guitar: Acoustic action height

For acoustic guitars, our recommendation bumps up to 7/64th of an inch (2.78mm)
on the bass side and 5/64th of an inch (1.98mm) on the treble side.

A comfortable ball park action set for me with an acoustic is around 5/64s Low E
4/64s high E. I use 12 gauge strings

```sh
# String Height at the 12th Fret:

# Low
Low E: 2.0mm (0.079")
High E: 1.5mm (0.059")

# Medium
Low E: 2.5mm (0.098")
High E: 1.8mm (0.071")

# High
Low E: 3.8mm+ (0.149")
High E: 3.2mm+ (0.126")

# Fractions	  Decimal	  Millimeters
  6/64        0.09375     2.38125
  5/64	      0.0781      1.984
  4/64        0.0625      1.5875
```

Measured on my blue acoustic

    low E = 3.0mm, high E = 2.25mm

Billy strings currently playing with action at 6/64" and 4/64" at the 12th fret.

## Guitar: Acoustic string gauges

Based on Martin Guitar's 80/20 bronze acoustic guitar strings:

- Extra light: `E .010; B .014; G .023; D .030; A .039; E .047`
- Custom light: `E .011; B .015; G .023; D .032; A .042; E .052`
- Light: `E .012; B .016; G .025; D .032; A .042; E .054`
- Medium: `E .013; B .017; G .026; D .035; A .045; E .056`
- Extra light 12-string:
  `E .010/.010; B .014/.014; G .023/.010; D .030/.012; A .039/.018; E .047/.027`

```bash
# In the elixir line of 80/20 bronze strings:
light = 012-053
custom light = 11-52
medium = 13-56

# ernie ball
extra light = 10-50
light = 11-52
med-light = 12-54
med = 13-56

# Martin
med = 13-56
extra-light = 10-47
light = 12
custom light = 11
medium = 13

# D'addario
light = 12-53
med = 13-56
custom light = 11-52
super light = 09-45
extra light = 10-50
light = 11-52
```

Overall the size names seem to be:

```
super-light = 09-45
extra-light = 10-50
light = 11-52
med-light = 12-54
med = 13-56
```

## String types

- 80/20 bronze
    - corrodes faster than phosphor
    - technically brass string
    - scooped mids sound - can be good to leave space for vocals
    - where the only option for decades
- phosphor bronze
    - 92% copper, 8% tin, trace phosphorus
    - last longer, corrode slower
    - more pronounced mids
    - are more popular now
    - doesn't leave the 1-3k frequencies as free for vocals as 80/20

## Acoustic guitar research

```
# From driftwood guitars ideals

Low e 12 fret 80 thou of inch= 2.032 mm
High e 12 fret 60 thou = 1.524 mm
```

```
# Classical guitar standard action

2.8mm treble (4mm here very hard to play)
3.8mm bass (4.3mm here very hard to play)
```

Action too low can have buzzing and you lose sound projection

## Neck relief

Procedure to measure:

1. capo on first fret
1. guitar in playing position
1. hold down near last fret
    - some say hold at fret where neck joins body (or just always at 12th fret)
        - I think this might make more sense if the guitar has no cutaway
          because the body joint is kinda the last usable fret so that's the
          portion of the neck you care about
1. measure under 6th string

The targets can differ depending on which fret you hold down

    music nomad say target = 0.008 (0.2 mm) if you hold at 12th, measure at 6th

You can also measure with a straight-edge instead of the string
