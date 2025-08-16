# Kali

## Via virtualbox

I try to treat kali as much of an "appliance" as possible - I don't want to
waste time faffing around with customising it.

- I started with the virtualbox ova version
- I had to disable USB controller because it wasn't importing properly with it -
  fixable I'm sure but have no use for it so didn't bother
- don't forget that scrolling works the other way round to macOS

1. Install virtualbox guest additions
    ```
    apt-get update
    apt-get install -y virtualbox-guest-x11
    shutdown -r 0
    ```
1. update packages
    ```
    apt-get upgrade
    apt-get autoremove
    ```
1. disable screensaver
    - `go to top right of screen dropdown -> Settings -> Power -> Power saving`
1. apt-get upgrade
    - I did a dist-upgrade and kali ran very slowly after so that's not worth
      the hassle IMHO
1. optional: disable semi-transparent background in terminal

## Terminal shortcuts

```
new tab     = ctrl+T
close tab   = ctrl+W

(fn+up arrow is page up on mac)
(fn+down arrow is page down on mac)

next tab       = fn+ctrl+up
prev tab       = fn+ctrl+down
move tab left  = fn+ctrl+shift+down
move tab right = fn+ctrl+shift+up
alt+1          = chooose tab 1
alt+2          = chooose tab 2 etc.

paste       = ctrl+V
zoom in     = ctrl++
zoom out    = ctrl+-
normal zoom = ctrl+0
```

## Tips

When setting up kali

1. disable screensaver
    - Go to settings -> privacy -> disable screenlock
2. setup aliases
    ```
    # handy for pasting content between VM and host
    alias pbcopy='xclip -selection clipboard'
    alias pbpaste='xclip -selection clipboard -o'
    ```
