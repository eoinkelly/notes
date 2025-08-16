# Home recording

## Terminology

1. Line level
    - Professional Audio +4 dBu
    - Consumer Audio -10 dBV
    - the loudest of the signals
2. Mic level
    - roughly -30 dBu
    - lowest volume of all the signals
3. Instrument level
    - roughly -20 dBu

## Direct input boxes

Types of DI box

-   Passive
    -   just passes signal
-   Active
    -   needs phantom power
    -   can add gain
    -   typically has more options

They are used as a fancy Y splitter:

-   input from instrument
-   there is a "through" output that you send to your amp
-   there is an XLR output that you send to your interface
-   lets you listen to the amp and record the DI at the same time.
    -   then you can re-amp the DI signal later

:question: does it get powered via phantom power from the XLR?

## -18 DbFS

The "ideal recording level" for plugins VU meters can be set to have it as a reference

Most people recommend setting your levels somewhere between -12dBFS and -20dBFS.

A lot of plugins, especially analog modeling plugins, borrow some concepts from analog gear. In the
hardware world, it is ideal to send a signal of around +4dBu to the next piece of processing gear.
This ensures a strong signal with a relatively low noise floor.

As seen above, the equivalent of +4dBu in the Full Scale is -20dBFS. Many plugin makers use -12dBFS
to -20dBFS as the sweet spot for the signal being driven into the plugin.

Using the sweet spot target for gain staging ensures a strong signal is sent to any plugins or
devices in your processing chain.

Unity Gain is a pretty simple idea, the level of what goes in must match the level of what comes
out.

Unity gain: The level going out must match the level coming in If we are setting our audio to the
sweet spot at every gain stage then we are effectively setting unity gain at each plugin.

This is a huge advantage for A/B testing different presets or plugins.

Our ears have a nasty habit of telling us that if something is louder then it is better. Therefore,
if one preset is louder than the other we are comparing then our brains will automatically think the
louder one will sound better.

By gain staging, we remove this natural bias and can compare before and after tests much more
effectively.

### dBFS

-   dBFS stands for decibels relative to full scale,
-   a unit used to measure amplitude levels in digital systems.
-   It's used to quantify the amplitude of a sound wave
-   0 dBFS representing the maximum level.
-   All other measurements in dBFS are negative numbers.

a signal that reaches 50% of the maximum level has a level of −6 dBFS, which is 6 dB below full
scale

dBFS is not defined for analog levels, according to standard AES-6id-2006. No single standard
converts between digital and analog levels, mostly due to the differing capabilities of different
equipment.

Conversion to analog levels

BBC specification: −18 dBFS = PPM "4" = 0 dBu

Audio waveforms have the highest point, a midpoint or average and the lowest point. The highest
point or maximum is referred to as the “peak”. The maximum peak that is allowed in digital audio is
0 dBFS (Decibels Relative to Full Scale). Similarly, the average point is called “Root Mean Square”
or more commonly, “RMS”. RMS lies between the loudest and the quietest portion of the audio. The
importance of RMS is that it measures how loud we perceive the sound to be. As the sound gets
louder, RMS draws closer to 0 dBFS. However, the perfect amount of “loudness” for mastering is not
solely based on RMS. The difference between the maximum and minimum RMS, known as dynamics, is the
best way to measure loudness in digital audio.

Headroom in digital audio refers to the difference in decibels between highest level “peak” and 0
dBFS. Although very commonly confused, headroom is not between the RMS (average level) of the mix
and 0 dBFS. Headroom is a “safety zone”, and it is often advised to leave headroom for mastering.
This implies that a finished audio file should peak at some decibels below 0 dBFS, else the task of
mastering music becomes very difficult for the Mastering Engineer.

As mentioned previously, 0.0 dBFS is the point where clipping starts to occur(in digital domain).
Keep the peaks in the mix below -0.0 dBFS. A good buffer and safe zone is -6dB FS.

Aim for a “fishbone” shape for the waveform so that the transients and dynamics are intact and even
transients do not go above -6dB FS.

https://unity.neuraldsp.com/t/optimal-input-level-for-highest-accuracy-when-using-ndsp-plugins/11048/2
