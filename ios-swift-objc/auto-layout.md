# Auto layout

- Autolayout is the computing of CGRect frames based on constraints you specify
- autolayout uses an "alignment rectangle"
    - which is defined by **layout attributes**
        - layout attributes
            - width
            - height
            - top/bottom/left/right
            - CenterX/CenterY
            - Baseline
            - Leading/Trailing
- every view has an alignment rectangle
- aligment rectangle vs frame
    - often they are the same
    - frame encompasses the entire view
    - alignment rectablne only encompasses the content you want to use for
      alignment purposes
- you cannot define the alignment rectangle directly
    - you provide hints (constraints) and the system figures out the alignment
      rectangle at runtime because it knows more than you e.g. screen size
- constraints can be used to give a view a fixed size

## Finding the nearest neighbour

Contraints are specified between a view and its "nearest neighbour"

Rules for finding the nearest neighbour

1. the closest sibling view in the specified direction (up, down, left, right)
    - mentally extend the **full edge** of the view in a particular direction
      until it hits a sibling - that sibling is the nearest neighbour
1. if a view doesn't have any siblings in the specified direction then the
   nearest neighbour is its superview (container)

Constraints can be added in IB or in code

## Intrinsic width and height

- Views have an "intrinsic" size - the size their content wants them to be e.g.
    - big enough to fit all the text in a UILabelView
    - big enough to fit the image in an UIImageView
- Intrinsic width and height are _implicit_ size constraints
    - => they are overridden by any explicit constraints you provide

## Layout attributes

Views have 11 kinds of "layout attributes"

An attribute is one of

1. left ??? is this the distance between left edge of view and nearest neighbour
2. leading (same as _left_ except senstive to language direction)
3. right
4. trailing (same as _right_ except senstive to language direction)
5. top
6. bottom
7. width
8. height
9. centerX
10. centerY
11. baseline

- trailing and leading do the correct thing when the language direction changes
  (unlike left and right)

## Anatomy of a constraint

- Type (one of the attributes above)
    - seems to be akin to "algorithm"
- First item
- Relation
- Second item
    - Not on all constraints have a second item e.g. "Width constraint" does not
      have one
- Constant
- Priority
    - Higher priority constraints are satisfied first
- Mutliplier
- Placeholder
- installed
    - yes|no

- Examples of constraint types

- Horizontal space
- Vertical space
- Equal widths
- Equal heights
- Top alignment

Things that can be an "item" in a constraint

- View width
- View horizontal center
- View vertical center
- View height
- Layout guide bottom

- Note that the item is not the "view" itself - it is an attribute of the view
- The page has to be able to solve the equation for the (width, height, x, y) of
  every box on the page.
- Xcode has ways to make constraints for multiple things at the same time but
  each constraint is between at most two things
- NB: Constraints are cumulative, and do not override each other.
- Constraints can cross the view heirarchy but there can be "barriers"
    - TODO: e.g. ???
- _Leaf views_ like buttons have an "instrinsic size"
    - tells the layout system that the view contains some content that it does
      not "understand" so it has to go off the intrinsic dimensions
- You can add/remove/edit constraints at run-time using the controller

What happens when you drag items onto a storyboard?

- When you first drag an element onto the storyboard it has no constraints.
- When you build, Xcode will fix the element's width, height and pin its
  position relative to the **top left corner** of the superview.

# Buttons at bottom of auto-layout

## pinning button

- cmd-click on each view to select a group of them and then hit the "pin" button

# Communicating between storyboards and code

A "connection" lets one object know where another object is in memory so it can
reference it - I think this means that "xcode connections" are just pointers ???

- Code references views via "outlet"
- view references code via an "action"

adding @IBOutlet is an instruction to _XCode_ (not swift or any compiler) to
make this variable available in Interface builder
