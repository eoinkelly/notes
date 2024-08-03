# Apple Voiceover

-   Voiceover has it's own focus cursor which is separate to keyboard focus
-   the VO focus jumps between controls
-   Controls are organised into groups which you can go into and out of explicitly
-   `VO-u` opens the rotor which is important for navigating web pages
    -   the rotor is a modal set of menus that you can navigate with arrows or just type to filter
    -   holds all the voiceover commands
-   VO key is `Capslock` or `Ctrl+option`

```
Cmd+F5 = start/stop voiceover

VO-arrows = move around
VO-shift-up = move out of group
VO-shift-up = move into group
VO-u = open the rotor, then arrows to navigate
VO-m = go to mac menu bar
vo-m-m = go to status things in menu bar

VO-k = enter/exit "speak the name of each pressed key" mode - use when keyboard is unfamiliar
VO-cmd-shift-rightarrow = open speed, voice controls (up/down arrows to change, repeat right arrow to cycle menus)
VO-z = repeat last said thing
VO-h = open root help menu
VO-h-h = open commands submenu in help
```

Has 2 navigation modes for we

1. DOM
2. Group

On Safari web content the rotor cycles between

1. Window spots
    * shortcuts to different parts of Safari's UI (not web content)
2. Links
3. Headings
4. Form controls
5. Landmarks


arrows are used for navigating the rotor


we can add keyboard shortcuts to pages which will be announced when they exist if that option is configured

Changing navigation mode (can also be assigned a shortcut key):
    ```
    VO-h-h
    type: dom
    then toggle web navigation DOM or group
    ```

how does keyboard focus and VO focus interact in web pages?

> By default, the VoiceOver cursor and keyboard focus are synchronised—set to
> follow (or “track”) each other. This is called “cursor tracking.” You can also
> set the VoiceOver cursor and the mouse pointer to track each other.