# Keyboards

- https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent
- https://www.youtube.com/watch?v=jLqTXkFtEH0

## Things which count as keyboards

- on-screen keyboards
    - swipey keyboard things
    - autocorrect
    - handwriting recognition
- voice dictation
- USB keyboards
- handwriting recognition hardware
- ?? others

## Step 1: Keyboard device <--> Kernel driver

- PS/2
    - tells the computer when a key is pressed
    - has inherent N-key rollover (NKRO)
- USB HID (Human Input Devices)
    - computer periodically (usually 100 times/sec) checks with keyboard to see
      which keys are being held \*responds with list of max 6 keys (+ modifiers)
      being held
    - protocol defined in the USB HID specification
- ADB (Apple Desktop Bus)
- ??? what happens with on-screen keyboards - presume they are just in the
  kernel so OS takes care of it all

Protocols are not necessarily 1:1 with the kind of cable/connection used to
connect the keyboard

## Within the Operating System

Input: a key press from the kernel Output: a decision about what action to take
if any

    How is the key mapped to a character?
    What steps are in the process?
    locale matters

## Web browser

You get "the thing printed on the key" and "they key" as separate parts of the
keyboard/input event

- "low level" key events
    - Events
        - keydown
        - keyup
        - keypress (obsolete, poorly specified, do not use)
    - These are mapped mostly to the idea of the input "device"
    - You probably don't need these
- "Higher level" input events
    - There are also higher level events which are potentially more reliable
    - Events
        - input
        - beforeinput
        - cmopositionstart
        - compositionupdate
        - compostiionend
        - selectionchange
