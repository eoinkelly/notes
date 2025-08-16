https://developer.apple.com/library/content/documentation/Cocoa/Conceptual/PasteboardGuide106/Articles/pbConcepts.html

Mac has 5 standard clipboards

1. general
1. ruler
1. find
1. font
1. drag (not accessible by `pbcopy` and `pbpaste`)

Clipboard data can be in one of 4 formats

1. plain text
1. EPS
1. RTF
1. RTFD

(maybe other custom formats possible too - I'm not sure)

A pasteboard can hold multiple representations of the same pasteboard item.

For example, a rich text editor might provide RTFD, RTF, and NSString
representations of the copied data. An item that is added to a pasteboard
specifies what representations it is able to provide.
