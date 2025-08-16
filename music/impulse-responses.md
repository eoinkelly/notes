# Impulse response

- Sources
    - https://melodiefabriek.com/sound-tech/everything-impulse-responses/
    - a digital representation of the sound of a speaker + mic + mic location +
      (presumably) the room
    - a digital representation of how a space reacts to an audio signal
        - can be used to analyse the acoustic properties of a space and optimise
          acoustics
    - sometimes called _convolution reverbs_
    - WAVE (.wav) files
        - sample size: Usually 24 bit
        - sample rates: varies
            - Examples
                - 44.2 kHz
                - 48 kHz
                - 88.2 kHz
                - 96 kHz
            - 44.2 kHz is enough because it captures frequencies up to 22 kHz
        - audio file length:
            - usually short
            - 200-500 ms is common (todo: verify)
- An IR filters the signal by convoluting it and is very responsive to input
  signals.
- An IR contains
    - the spectrum of all frequencies (20 - 20.000 Hz)
    - the loudness levels of all these frequencies
    - the length (sustain/ringing) of all these frequencies
- IRs are resampled by the DAW to match the sampling rate of the project
    - Garage band outputs only 44.2 kHz - it can re-sample inputs from other
      sampling rates
    - Logic can output at multiple sampling rates
- 2 methods of creating an IR
    1. Transient method
        - You record a transient like a pistol shot
        - -- Difficult to record
        - -- Doesn't contain a full spectrum of content
        - ++ IR needs no further processing before importing it into convolution
          software
    2. Sine wave sweep and deconvolution
        - record a sine wave that sweeps the audible range
        - the recording must be _deconvolved_ (division of signals)
            - find the reflections that are present across the entire recorded
              wave all all moments and time and every frequency
            - the reflections are time and level aligned to the beginning of the
              file
- If you had an IR which is an empty wav then the output of the convolution will
  always be 0 (so silence)
    - so `convolute(input, empty_wav) == empty_wav`
- An IR that contains all frequencies (20hz - 20khz) at the same level is called
  a _Dirac file_
    - it acts as the identity function in the convolution - output will be
      identical to input
    - so `convolute(input, dirac_file) == input`

## IR Loaders

- https://www.igniteamps.com/#nadir
    - Available as DAW plugin (VST/VST3/AU formats)
    - describes itself as a _convolver_
    - has high-pass and lo-pass filters to trim unwanted frequencies

## Aside Dynamic Speaker Response (DSR)

- Celestion proprietary technology.
- > Not simply a speaker’s frequency response but also its “attitude”,
  > delivering unprecedented accuracy and feel.
- Only compatible with their
  [Celestion SpeakerMix Pro](https://www.celestionplus.com/product/celestion-speakermix-pro/)
  plugin
    - 140 USD, includes 10 DSRs that you can choose for yourself

## Logic Pro's Impulse Response utility

- Let's you create IRs in Logic
- You can choose how many channels to record:
    - mono
    - _omni discrete_ ???
        - I think this is when you record one speaker but with multiple mics at
          the same time
    - stereo (but each channel processed together)
    - true stereo (each channel processed separately
    - 4-7 channels for surround sound
- When you have 4+ channels you can store as either
    - traditional multitrack
    - B-format encoding
- Has a sine wave sweep generator that generates a sine wave of increasing
  frequency
- The docs reference something about a "starter pistol"?
  https://support.apple.com/en-jo/guide/logicpro-iru/dev6326399ca/10.7.5/mac/12.3

## Sources of IRs

- You can download IRs and DSRs from
  [Celestion](https://www.celestionplus.com/products/guitar-responses-by-speaker/)
    - Each speaker has a library of IRs associated
        - it is recorded in multiple cabinets combinations with multiple
          microphones in multiple positions
        - each "library" is around 40 USD from their website
- There are IR communities online
- https://lancasteraudio.com/

## B-format surround sound encoding

- https://support.apple.com/en-jo/guide/logicpro-iru/dev022fbc493/10.7.5/mac/12.3
- Uses 4 audio streams to represent audio in 3D space:
    1. W = sound pressure
    2. X = front-to-back
    3. Y = left-to-right
    4. Z = top-to-bottom

## Convolution

https://theproaudiofiles.com/impulse-responses-and-convolution/ very good
article

> this process involves multiplication (or division in the case of
> deconvolution) of two audio signals to produce a third

> the method is used in IIR filters (infinite impulse response) which are
> capable of filter slopes of 12 dB per octave & above, and FIR (finite impulse
> response) filters that can produce a 6 dB per octave slope. Convolution lies
> at the heart of FFT or fast Fourier transform processes as well, which include
> time expansion and compression independent of pitch and vice versa. Processes
> like reverberation, modulation, cross-synthesis and filtering all use
> convolution

- Every frequency of the input signal will be convoluted in a way which vaguely
  reminds of a lookup table.
- If you convolute a perfect sine wave of 440 Hz, the output signal will contain
  nothing but that 440 Hz frequency.
- An IR cannot distort a signal because of the direct relationship between the
  frequencies of the input and output.
- So no harmonic distortion and no aliasing will ever occur.
- Convolution is a 100% linear process so it can't compress the input.
- This results in a very dynamic and lively sound.

## Random

> A guitar speaker cabinet is known for its strong midrange and peak frequencies
> which are caused by the speaker and the reflections inside the cabinet. ???
