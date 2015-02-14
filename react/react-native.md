# React native

* JS on background thread
* does the "change diffing" to a native view layer (native view elements exposed as React components e.g. `UIButton -> <Button>`
* rendering happens "off the main thread"
    * QUESTION: how ???
* "learn once write anywhere"
* unclear whether same code will run on both platforms
    * it seems to just be a new way to make native apps
    * instead of ObjC/swift use JS
* Already in use by FB for some of their apps
* No DOM, no webview, just JS as a native app development language


* QUESTION: Debugging ???
* QUESTION: Interop with existing native APIs that are not views ???

Reasons for react native

* Touch handling is much better on native
    * they don't go into this much in the videos
* Native components exist and are, in their opinion, always better than alternatives
    * instead of outputting DOM elements it outputs UIView classes
    * the views are set on the main thread, the diff algorithm runs on a different thread
    * they run jscore
    * it is possible to run the JS that powers the app remotely via websockets!
* layout is very different across ios, android, web
    * from gpu pov layout is just setting top, left, width, height
    * they think ios native layout is mostly maths, even the linera constraint solver has problems e.g. can't do line breaks
    * they think flexbox is the best way to do layout
    * there is NO webview
    * they have created their own parser that will parse flexbox and create a similar tree of nodes that have w,h,top,left
    * they re-implement a subset of CSS - they focused on the subset they could make fast, and were most useful
