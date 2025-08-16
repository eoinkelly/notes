# USB and Thunderbolt

## USB 3 vs Thunderbolt 3 vs wtf

- There is now one **cable** with "USB Type C" connectors on either end which
  can carry any of these data protocols:
    1. USB 3.1 Gen 1 - 5 Gbps
    1. USB 3.1 Gen 2 - 10 Gbps
    1. Thunderbolt 3 - 20-40 Gbps (depending on how many PCIe lanes are
       dedicated to the port by the device)

## Specific devices

### Apple Thunderbolt 4 Pro Cable

- https://support.apple.com/en-nz/118204
- It supports Thunderbolt 4, Thunderbolt 3 and USB 4 data transfer speeds up to
  40 Gbps and USB 3.2 Gen 2 data transfer speeds up to 10 Gbps.
- This cable also connects to Thunderbolt 4, Thunderbolt 3 or USB-C devices for
  charging your Mac laptop computer, iPhone or iPad. It delivers a maximum of
  100W power to any connected device.

## Active vs passive cables

Cables can be active or passive. Passive cables cannot carry the full data rate
at longer lengths.

## USB

- A connector port can be identified with the 3 tuple
  `(usb-data-protocol, type, format)`
    - Data protocol examples are _USB 1_, _USB 2_ etc.
    - Type refers to the physicality (shape of the connector on the end of the
      cable) e.g. _USB Type A_, _USB C_ etc.
    - Format is a sub-category of the type e.g. _USB Type A Mini_, _USB Type B
      Regular_
        - Each type can have many formats
- Type + format defines
    - the exact shape of the connector
    - the shape of "the pipe" while USB-{1,2,3} defines a data protocol which
      can flow through the pipe
- Implementers don't necessarily have to support the whole protocol to have
  their devices "support USB X" e.g.
  https://en.wikipedia.org/wiki/USB4#USB4_peripheral_device
- Examples:

    ```
    # This is incomplete
    USB 1
        Type: USB Type A
            Format: Regular
            Format: Micro
            Format: Mini
        Type: USB Type B
            Format: Regular
            Format: Micro
            Format: Mini
    USB 2
        Type: USB Type A
            Format: Regular
            Format: Micro
            Format: Mini
        Type: USB Type B
            Format: Regular
            Format: Micro
            Format: Mini
    USB 3.0
        ???
    USB 3.1 Gen 1 (aka USB 3.0)
        Type: USB Type C
            (Only one format)
    USB 3.1 Gen 2
        Type: USB Type C
            (Only one format)
    ```

- _USB 3.0_ is almost identical to _USB 3.1 Gen 1_ - they have identical
  transfer rate of 5 Gbps
    - Eoin: I think USB 3.0 was retroactively re-branded USB 3.1 Gen 1 by the
      USB group
    - TODO: how are they different?
- USB Type C connectors supports power delivery up to 100W (I think - check
  this)
- USB history
    - USB 1: 12 Mbps in one direction, less in the other
    - USB 2: 480 Mbps in one direction, less in the other
    - USB 3.0/3.1 Gen 1: 5 Gbps in both directions
    - USB 3.1 Gen 2: 10 Gbps in both directions
        - Type C connector introduced
    - USB 3.2: can be:
        - 10 Gbps symmetrical in USB-C and earlier connectors
        - 20 Gbps symmetrical in USB-C only
    - USB4
        - Based on Thunderbolt 3 protocol
            - but that does not mean that all USB4 devices support the
              Thunderbolt protocol - they might only support USB4 which is
              different from their pov.
        - USB4 V 1.0: Gen 2 added an 80 Gbps
        - USB4 V 2.0:
            - added an 80 Gbps mode

> USB-IF intends only for the new, bandwidth-based logos and names to be used
> with consumers. And for cables, the type (passive, active) and the highest
> supported bandwidth are usually enough to uniquely identify a cable and its
> supported features

### USB alternate modes

Alternate modes allow a USB cable to pass data that isn't in USB format. The USB
data protocol stream can be replaced by one of:

- Thunderbolt 3 or 4 data stream
- DisplayPort data stream
- HDMI video data stream
- PCI express stream

## Thunderbolt

- versions 1 and 2 got very little adoption
- Thunderbolt 3
    - supports USB-C connection
    - the thunderbolt protocol is sent instead of USB protocol data in an
      "Alternate mode"
    - two thunderbolt controllers can communicate over a USB-C cable
        - the cable and connectors are "USB" but nothing about the data passing
          is - it's thunderbolt
- Thunderbolt 4
    - min 20 Gbps but up to 40 Gbps
    - not faster than 3 but supports longer cables and more monitors
    - requires USB-C for all connections
    - passive cables only 40 Gbps up to 0.5m, active cables 40 Gbps up to 2m
- Thunderbolt 5
    - Thunderbolt 5 is an implementation of USB4 "80 Gbps".

## Use cases

### 1. computer to monitor cable

needs to transfer data fast enough for the resolution you want big power
delivery is nice if you don't connect for long so your laptop can charge faster
but it doesn't matter if you connect all day

### 2. charging cable for devices

we care about power only, it's handy if it can transfer some data but speed not
a big deal

### 3. external hard drive

power not a biggie but data transfer speeds need to support what the drive can
support
