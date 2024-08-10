# Audio plugin formats

https://www.sweetwater.com/sweetcare/articles/which-plug-format-need-for-my-daw/

native => CPU does the work not some outboard gear

1. VST
    * Introduced by Cubase in 1996
    * VST3 is latest version
    * Windows and macOS
    * Mostly widely supported by DAWs
    * DAWs
        * Ableton live
        * Cubase
        * FL Studio
        * One Reason
        * Reaper
        * Neudo
2. Audio Units (AU)
    * macOS only
    * made by Apple
    * part of CoreAudio
    * Logic Pro, Abelton Live, Studio One
    * This is the only format that Logic Pro supports
3. AAX (Avid Audio Extension) Native
    * 2 versions
      * AAX DSP
      * AAX Native
    * Introduced by Avid as part of Pro-tools
    * Pro-tools 10 and later
    * Windows & macOS
4. Real time AudioSuite (RTAS)
    * Pro-tools 10 and earlier only
5. Standalone
    * ? presumably these are one of those underlying plugin format wrapped in an app?

## AudioUnits (AU) format plugin

macOS AU format plugin locations:

```
/Library/Audio/Plug-Ins/Components (Amplitube installs here)
~/Library/Audio/Plug-Ins/Components
```
