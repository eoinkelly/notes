# EQ (Equalisation)

EQ is very important in metal guitar sounds

## Shelving EQ

- Takes a flat EQ curve and creates 3 areas (lows, mids, highs)
- Lets you add/remove gain from each of the 3 areas
- You can choose the slope of the transitions between areas
- They create a natural sounding effect

## Graphic EQ

- Lets you graphically see an EQ curve
- Has a fixed number of bands
- lets you adjust the gain of each by (in general) -15dB to +15dB
- Boss have
    - GE-7 (7 band graphic EQ)
    - GE-10 (10 band graphic EQ)
    - the band center frequencies are fixed and they dou
    - each center frequency is 2x the one below it e.g (in Hz)
      `31 62 125 250 500 1k 2k 4k 8k 16k` (these frequencies seem common)
- Graphic EQs have a fixed Q (so a fixed width of frequencies that get
  boosted/cut)
- Often have an overall level you can set to boost/cut all frequencies at same
  time
- Sometimes they have a "proportional Q" design - small boosts use a wide Q,
  larger boosts narrow the Q
    - Do pedals have this or is it only rack gear?
- An advantage of the fixed center frequencies and Q values it means you can
  undo what you did by using the reverse settings - this is a lot harder to do
  with other kinds

## Parametric EQ

- You can choose Q (how wide a band of frequencies to boost/cut)
    - Sometimes it's a toggle from `narrow <-> medium <-> wide`
- A -12dB to +12dB gain is common
- Often have an overall level you can set to boost/cut all frequencies at same
  time
- has a fixed number of bands (e.g. 3)
    - but the center of each band can be moved so you get a lot of control
    - each band has a controllable Q, center frequency and gain
- Pedals
    - [Puppet Master EQ](https://www.mastereffectspedals.com/pmeq)
        - A parametric EQ pedal
        - Designed to mimic the EQ gear used during recording
    - Master Effects EQ From Hell
        - Low: 33-300 Hz
        - Mid: 250 -2500 Hz
        - High: 1.1-11 kHz
        - Notice the overlap in the bands
        - Each band has a Q, center freq and gain control
- DAWs
    - started as rack hardware but usually has a frequency visualiser in DAWs
    - you can choose filter shapes, proportional Q's, and add as many bands as
      you want
    - bands can interact and overlap
    - tend to be clean and precise, they don't add color/tone/texture to a
      signal
    - some have options to let you apply a different EQ to different sides of a
      stereo channel
    - can lead you to choosing the curve with your eyes not your ears so
      disabling the visualisations can be good

Tip: Parametric EQs are better used in the effects loop because they can impact
the signal more there

## Linear phase EQ

- All the above EQs adjusts the phase of the signal too
    - when you boost/cut you do adjust the phase
    - not usually a problem unless you have lots of bands
- Linear phase equaliser maintains the phase relationship
    - -- much higher CPU usage
    - -- can create artefacts such as ringing

## Dynamic EQ

- A parametric EQ where the amount of boost/cut is linked to the input signal
  e.g. filter more if input is louder
- Dynamically activated parametric EQs

## Automatic/intelligent EQ

- AI/ML based
- attempts to automatically "improve" the signal

## Frequency band names

- Low
    - Good: Bass
    - Bad: Rumble
- Low-mid
    - Good: Warmth
    - Bad: Mud/boxy
- High-mid
    - Good: Clarity
    - Bad: Harsh
- High
    - Good: Presence
    - Bad: Fizz
