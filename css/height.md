# height

- specifies height of the \_content_area (inside padding, border, margin areas)
- The min-height and max-height properties override height.
- If the height of the containing block is not specified explicitly, and the
  element is not absolutely positioned, the value of its height computes to
  `auto` (it will be as tall as the content inside it is, or zero if there is no
  content).

- Consequences
    - similar to min-height any value you put in here is coerced to `auto`
      unless the containing block has an explicit `height` (unless you are
      absolutely positioned)
    - no height on containing block && not abosolutely positioned => computed
      value is always `auto`

- If the elements content portion requires more vertical space than available
  from the value assigned, the elements behavior is defined by the overflow
  property.
    - so "overflow" is triggered if the intrinsic height of the content is
      bigger than the height you specify

When using the keyword auto, height is calculated based on the elements content
area unless explicitly specified.

This means a value based on a percentage is still that of the elements content
area.

Similarly, if the height of the container is set to a percentage value, a child
elements percentage height is still based on the content area of that child
element.

## percentage heights

- The percentage is calculated with respect to the height of the generated box's
  containing block.
    - so XX% means be XX% of the height of your "containing block"
    - if the height of your containing block is not specified explicitly then
      whatever value you use will be computed to 'auto'
        - => to set the height of block you must also have set the height of its
          containing block
        - exception: if this element is absolutely positioned

A percentage height on the root element is relative to the initial containing
block.

Note: For absolutely positioned elements whose containing block is based on a
block-level element, the percentage is calculated with respect to the height of
the padding box of that element.

# min -height

- It prevents the _used_ value of the height property from becoming smaller than
  the value specified for min-height.
- The min-height property always overrides both height and max-height
- applies to all elements except non-replaced inline elements, table columns,
  and column groups (i.e. colgroup, col ).
- % values
    - The percentage is calculated with respect to the height of the generated
      box's containing block.
    - If the height (note it is _height_ not _min-height_) of the containing
      block is not specified explicitly (i.e., it depends on content height),
      and this element is not absolutely positioned, the percentage value is
      treated as '0' (for 'min-height') or 'none' (for 'max-height').
        - so whatever % value you give min-height will be coerced into '0' if
          the containing block does not have an explicit `height` set.
- consequences:
    - min-height of a child block will only work if there is `height` set on the
      containing block

# Aside: Terminology

Initial value Computed value Used value
