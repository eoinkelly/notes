# Logical properties and values

- Spec: https://drafts.csswg.org/css-logical/
- The writing modes spec outlines some of the basic ideas this builds on
    - https://www.w3.org/TR/css-writing-modes-4/
- Support: 2021-04-27: all modern browsers except Safari but it is in Safari TP
- what:
    - flow relative properties
    - flexbox and grid already use this style
    - a set of properties which control layout for logical not physical pov
    - a set of properties for controlling layout i.e.
        1. directions
        2. dimensions
    - physical: left/right/top/bottom
    - logical: start/end
    - if you use `writing-mode` and logical properties then the layout should
      "just work"
        - does it?

There are two dimensions

the "inline" dimension matches how text runs in the currently active
`writing-mode` English: horizontal, left to right Arabic: horizantal, right to
left Japanese: vertical, ? to ?

The "block" dimension is the dimension in which blocks such as paragraphs
display in

English: vertical Arabic: vertical Japanese: horizontal

If the writing mode is for english, then

Inline dimension = horizontal Block dimension = vertical

properties

From https://dev.to/argyleink/5-css-predictions-for-2020-pl3

height/width 👉 block-size/inline-size margin/padding 👉
margin-block/margin-inline & padding-block/padding-inline borders 👉
border-block/border-inline overflow 👉 overflow-block/overflow-inline text-align
👉 start/end float 👉 block-start/inline-start/etc
