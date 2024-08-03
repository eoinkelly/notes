## MIDI

-   sources
    -   https://en.wikipedia.org/wiki/MIDI
-   MIDI (Musical Instrument Digital Interface) is a technical standard that describes
    1. a communication protocol
    2. digital interface
    3. electrical connectors
    4. a file format
-   History
    -   Prior to MIDI controllers and synthesisers from different manufacturers were not compatible
    -   Standardised in 1983
-   Version 1.0 Spec
    -   General
        -   2.0 extends but remains compatible with 1.0
        -   There are also extensions to the core 1.0 spec that are not part of 2.0
    -   Constrains
        -   Uni-directional only! You need a second cable to send messages the other direction
        - Since MIDI cables are uni-directional, devices do not need to negotiate about who is sending data
            - Q: How does this work over USB?
    -   Physical
        -   Max cable length 15m
        -   MIDI devices have _Opto-isolators_ to keep them electrically separated from their MIDI connections and avoid ground loops
    -   Thru port
        -   Many devices have both MIDI input and MIDI output ports
        -   But they don't copy messages from input to output
        -   A _Thru port_ forwards everything received on the input port
        -   Data transmission is serial
            -   it can only send one event at a time
            -   if multiple channels want to send an event at the same time, they have to be put in a queue
            -   e.g. 3 byte MIDI message takes 1 ms
                -   if device has 16 channels then one channel could have to wait 16 ms to send its event
                -   Audible timing errors due to this are called _MIDI Slop_
                -   MIDI interfaces which support Multiple-in Multiple out (MIMO) exist to avoid this
        -   A single MIDI cable can carry up to sixteen channels of MIDI data,
            -   each of channel can be routed to a separate device.
    -   Protocol
        -   No error detection in the protocol
        -   MIDI router boxes are a thing to avoid the delay inherent in chaining many devices
    -   Connectors
        1. Uses 180deg 5-pin DIN connector
            - Visually looks somewhat like pre-USB mouse and keyboard connectors
                - 5 wires available, **typically only 3 used: ground + midi signal carried on balanced pair**
                - Sometimes other pins used to supply DC power (aka phantom power)
        2. TRS minijack
            - Some devices used TRS to save space (remember: only 3 wires necessary for MIDI)
            - MIDI-over-minijack standards recommend using 2.5mm connectors to avoid confusion with 3.5mm headphone/speaker connectors
                - Many manufacturers do not follow this
-   MIDI to USB
    -   transfers MIDI over USB
-   MIDI Event
    -   Each interaction with a key, button, knob or slider is converted into a MIDI event
    -   includes musical instructions, such as a note's pitch, timing and loudness.
-   Data transfer is over one of
    -   MIDI cable
    -   USB cable
    -   Ethernet cable
-   MIDI controllers
    -   A MIDI controller generates MIDI events
    -   Often a keyboard
        -   MIDI was designed with keyboards in mind
        -   Any controller which is not a keyboard is an "alternative" controller
            -   Guitar and drum controllers are included here
    -   2 types of controller:
        1. performance controllers
            - sends notes
            - used to perform music
        2. non-performance controllers
            - does not send notes
            - sends other kinds of real-time events
    -   Many real devices are a combination of both kinds of controller
-   MIDI instruments
    -   contains
        -   ports to send and receive MIDI signals
        -   CPU to process them
        -   audio circuitry to generate sound
        -   a UI to allow users to interact with the instrument
        -   often factory stored sounds in ROM
        -   often a screen for human to interact with
    -   some devices include both controller and instrument(s)
    -   Examples
        -   Synthesizers
            -   sound generators
            -   -   might include a keyboard or support an external MIDI controller
        -   Samplers
            -   record audio and store it
            -   control playback and effects
        -   Drum machines
            -   contains a sequencer to let you create drum patterns
        -   Guitar pedals
-   MIDI File format (SMF)
    -   usually has `.mid` extension
    -   saved music sequences
    -   binary format
    -   starts with ASCII `MThd`
    -   is a serialisation of a stream of midi messages into a file
-   System exclusive messages (SysEx)
    -   sends info about synthesizers functions instead of performance data
    -   basically they are extensions to Midi
    -   each manufacturer has a unique ID number which is included in SysEx messages
    -   sometimes the instrument ID will also be in a SysEx message so a controller can address multiple targets
-   Universal System Exclusive messages
    -   A subset of SysEx messages
    -   used for extensions to MIDI not intended to be exclusive to one manufacturer
-   MIDI implementation chart
    -   a device typically doesn't respond to all MIDI messages
    -   this chart documents what it does respond to

## MIDI in guitar pedals

Q: when a pedal supports midi, what does that mean?

-   Complex guitar pedals sometimes support MIDI
-   Example controllers
    -   ABY switchers
    -   Effect switching pedals like Boss ES5
    -   Pedalboard effects switchers
    -   Foot switches
-   You can usually configure what MIDI messages the controllers send
-   Usually have TRS jacks (either 3.5mm or 2.5mm)
-   Example: Boss DD-220
    -   https://www.dj-store.ru/data/files/files/2310_file.pdf
    -   Can use midi controller to switch between saved presets
    -   Can configure
        -   MIDI receive channel (1-16)
        -   MIDI transmit channel (1-16
        -   Whether PC message sending is on/off
        -   Whether PC message receiving is on/off
        -   Whether CC message sending is on/off
        -   Whether CC message receiving is on/off
            -   This is what allows you to control it with a foot switch or expression pedal
        -   You can configure the following "controllers", giving them values off, 1-31,64-95 (notice 32-63 and 96+ are unavailable - why?)
            -   Time CC
            -   Feedback CC
            -   E.Level CC
            -   Param CC
            -   Tone CC
            -   Mod CC
            -   On/off switch CC
            -   Memory CC
            -   Ctl1 CC
            -   Ctl2 CC
            -   Expression CC
            -   Effect of/off CC
        -   Whether pedal will sync to internal tempo or to a MIDI clock from another device
        -   Source of realtime messages can be internal or from MIDI IN
        -   Turn MIDI thru (passthrough from input to output) on/off
-   Example: Friedman IR-X
    -   https://friedmanamplification.com/wp-content/uploads/2023/10/Friedman_IRX-Manual-v2.2-Web.pdf
    -   has 5pin DIN MIDIinput
    -   Default channel is OMNI (IR-X will listen on all channels
    -   It is "midi programmable"
    -   Can store 128 presets of
        -   channel
        -   boost on/off
        -   fx loop active/bypass
        -   channel presence
        -   channel thump
    -   Steps to store a preset using the pedal:
        1. Send the chosen MIDI PC event/number/thing using your MIDI controller
        2. Adjust the settings on your IR-X
        3. Long-press the BOOST foot-switch to store it in the selected MIDI PC
    -   Preset can also be created using the software editor

## Extensions to MIDI standard

1. [General MIDI (GM)](https://en.wikipedia.org/wiki/General_MIDI)
    - also known as GM 1
    - provides a standardised sound bank so that when you use PC messages to choose an instrument, you know what you are getting on other systems
    - The (single) sound bank contains 128 sounds as 16 families of 8 instruments
    - Each instrument is assigned a program number
    - Percussion instruments are on channel 10
        - Q: how do channels interact with this?
    - Eliminates variation in note mapping e.g. what note should be middle C
        - in GM, note #69 plays A440 (440 Hz A) so middle C is #60
    - Compliant devices must offer
        - 24 note polyphony (24 notes playing at the same time)
        - respond to
            1. velocity
            2. aftertouch
            3. pitch-bend
        - support certain controller numbers for sustain pedals
        - A simplified _GM Lite_ is used by devices with limited processing
    - You can get "soundfonts" which implement these GM instruments
    - Standardising allowed MIDI stored in files to be exchanged usefully
2. GS, XG
    - Roland General Standard (GS) is an extension of GM
        - can select between multiple banks of 128 instruments
        - use MIDI Non-Registered Parameter Numbers (NRPNs) to implement this
        - is backwards compatible with GM
    - Yamaha extended midi (XG)
        - similar to Roland GS
        - is backwards compatible with GM
3. General Midi 2 (GM2)
    - An extension of GM from 1999
    - Polyphony is 32 voices
    - standardises controller numbers, RPNs
    - Incorporates MIDI tuning standard
4. SP-MIDI Scalable Polyphony MIDI
    - aimed at lower power devices
    - scaled down version of GM2
5. Tuning standard (MTS)
    - ratified 1992
    - MIDI defaults to equal temperament tuning
    - this allows alternate turnings and microtunings
6. Time code (MTC)
    - sequencer can drive a midi system with it's internal clock
    - MIDI clock is based on tempo but MTC is based on frames and is independent of tempo
    - MTC includes position info and can recover from drop outs
    - multiple devices with clocks can synchronise
7. Machine control (MMC)
    - a set of SysEx commands to operate the transport controls (play, pause, ffw, skip etc.) of recording devices
    - allows a sequencer to send commands
        - start
        - stop
        - record
        - fast forward
        - rewind
    - No synchronisation data provide but devices can use the _Time code MTC_ extension to synchronise
8. Show control (MSC)
    - a set of SysEx commands for sequencing and remotely cueing entertainment equipment such as lighting, music and motion control
    - transmits "cues" which can be from button push or sequenced
    - used in stage productions, museum exhibits, recording studios and amusement parks
    - many modern MSC devices use Ethernet as a transport now for bandwidth and availability reasons
    - example: https://help2.malighting.com/Page/grandMA2/remote_control_msc/en/3.9
9. Timestamping
    - send timestamps of the notes before the note events
    - lets the receive account for delays due to MIDI being serial and play back correctly
    - needs both hardware and software to support it
    - built into Firewire MIDI, Apple's CoreAudio and Linux ALSA
10. Sample dump standard
    - transport audio samples between instruments
11. Downloadable sounds (DLS)
    - DLS level 1 ratified 1997
    - allows devices to expand their "wave tables" with downlodable sound sets
    - DLS level 2 in 2006
    - files are in XMF format
12. MIDI Polyphonic expression (MPE)
    - released 2017
    - Assigns each note it's own MIDI channel so that controller messages can be applied to it
        - enabled pitch bend and other expressive control for individual notes
        - lets you do slides between notes, vibrato
    - Your controller and synthesizer must both support MPE to use it
        - but MPE devices fall back to regular MIDI
    - Controllers can map key pressure, location on key to various things e.g. volume, pitch etc.
    - lets you do things without having to program them
    - lets you do things that can't be done easily with sliders or knobs
    - GarageBand supports MPE

## MIDI messages

-   https://midi.org/midi-1-0-core-specifications
-   a control instruction sent from the controller to the receiver
-   5 types of MIDI message, organised in 2 categories
    -   Channel messages
        1. Channel voice
            - real-time performance data over a single channel
            - example channel voice messages:
                - note-on
                    - starts a note
                    - contains:
                        - note-on for channel number (144 - 159, one for each of the 16 channels)
                        - note number (specifies pitch, 0-127
                        - velocity (how forcefully the note was played, 0-127)
                    - see [CC messages reference](<Control Change Messages (Data Bytes).pdf>)
                    - one bit of each byte is protocol overhead so only 7 bits available
                    - note numbers are 0 - 127
                    - represents C\_-1 -> G_g (extends beyond the A_0 - C_8 is an 88 note piano)
                - note-off
                    - ends a note
                    - ways a note can stop:
                        1. note reaches the end of it's delay envelope
                        2. an explicit `note-off` command is received
                    - monotonic devices terminate the previous note when a new one arrives
                    - polyphonic devices can play notes simultaneously up to the devices polyphonic limit
                - program-change (PC)
                    - change a device's patch
                - control-change (CC)
                    - adjust an instrument's parameters
        2. Channel mode
            - define an instruments response to voice messages
    -   System messages
        1. System common
            - has no channel number
            - intended for all receivers
        2. System real-time
            - has no channel number
            - time sync messages sent here
            - transport control sent as these
            - status bytes only, no data bytes
        3. System exclusive
            - has no channel number
            - send info about a synthesizers functions
            - can carry proprietary manufacturer messages
            - MIDI Manufacturers association allocates manufacturer IDs which appear in SysEx messages
-   Message format
    -   made up of 8 bit bytes
        -   first bit of each byte is a flag for either status byte or data byte
        -   7 bits carry in information
-   Channels
    -   MIDI link has 16 channels numbered 1-16
    -   A device can listen to specific channels (called `OMNI OFF` or can listen to all channels (called `OMNI ON` mode)
        -   listening to all channels ignores the channel address of the message

## Web MIDI

-   sources
    -   https://developer.mozilla.org/en-US/docs/Web/API/Web_MIDI_API
    -   https://www.smashingmagazine.com/2018/03/web-midi-api/
    -   https://midi.org/specs (MIDI specs)
    -   https://webmidijs.org/ sugar for the Web MIDI API
-   works on everything except Safari as of Aug 2024
-   Interesting uses
    -   https://haeckse.codeberg.page/kato/
        -   https://codeberg.org/haeckse/kato (code)
        -   web app that can control the katana

