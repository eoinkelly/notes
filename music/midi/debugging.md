# MIDI debugging with Wireshark on macOS

- sources
    - https://midi.org/basic-of-usb

> In 1999, the MIDI specification was developed by the USB-IF in cooperation
> with the MIDI Manufacturers Association and included in the Audio class of
> devices.  That is why sometimes when you connect a USB-MIDI peripheral, the OS
> will display a message that says USB-Audio devices connected.  As far as USB is
> concerned MIDI is an Audio Class Compliant device.

- a MIDI _port_ is a group of 15 MIDI channels
- USB bandwidth allows multiple ports over the same cable but not all instruments support this
- The generic OS driver will support MIDI but you might need a custom driver to get all features
- Devices often have separate USB ports for sending and receiving MIDI

- In MIDI over USB
    One side must be a USB Host
    The other side must be a USB Device
- Implications
  - You can't connect USB "devices" directly together
  - This seems to be why the A-B cable is popular - USB A for the host, USB B for the device

Some devices can act as USB host _and_ USB device

Steps

1. Install Wireshark
2. Disable SIP (You have to disable SIP to do USB capture with Wireshark)
3. Capture USB traffic from the USB interfaces which appear in Wireshark

```sh
# dump info about USB host controller
ioreg -w0 -rc AppleUSBHostController
```