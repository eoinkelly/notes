# CSS Variables aka Custom Properties

Anatomy

- Property value
- Descriptor
- Declaration

## CSS Variables

Sources

- https://www.freecodecamp.org/news/everything-you-need-to-know-about-css-variables-c74d922ea855/

Overview

- A variable is any property whose name begins with `--`
    - the naming idea is that they are like `-webkit-blah-blah` but with a blank
      `webkit`
    - committee kept away from `$foo` so that you could use CSS variables and
      SASS variables in the same stylesheet
    - they are also called _custom properties_
- Use-cases
    - Theming
    - Creating variations of a widget
- Updating the value of a variable causes the bits of hte page which use it to
  immediately re-render
- variable names are case sensitive
- The normal cascade is used to decide which variable value applies if there are
  multiple declarations of the same variable
- you can define them in HTML `style` attribute
    - could be useful I guess but seems messy
- You can update them from JS exactly the same as any other CSS property
    - old-school way would have been to change classes but instead you can
      change the variable property
        - is that strictly better? it kinda moves some of the styling into the
          JS?
        - Maybe it's still better to change classes and let the classes change
          variable values
- Their value cascades
- Browser support: last two versions of all the major evergreens. Obvs no IE11

Questions

- It seems you can create cyclic dependencies - what happens if you do?

```scss
// Defining variables
// ******************
// Use :root to create globally scoped variables
:root {
    --main-color: #ff0000;
}

// otherwise variables are scoped to the **selector** they are declared in i.e.
// this variable applies to any element where this selector applies
.thing {
    --main-color: #00ff00;
}

// You can override them in media queries
:root {
    --gutter: 10px;
}

@media screen and (min-width: 100px) {
    --gutter: 30px;
}

// Referencing variables
// ******************

.other-thing {
    // you can use variables as property values
    color: var(--main-color);
    color: var(
        --main-color,
        black
    ); // you can add a fallback for the case where the variable not set

    // ... but not property names
    var(--side): 20px; // ignored by browser because invalid

    // you can't do math out of the box either
    --whitespace: 20px * 2 // Invalid
        // but you can by just adding the calc function
        --whitespace-base: calc(20px * 2); // works
}

:root {
    --size: 20;
}

body {
    font-size: var(--size) px; // ERROR you can't do this
    font-size: calc(var(--size) * 1px); // but this works
}
```

```scss
:root {
    --foo: var(--foo);
}

body {
    margin: var(--foo);
}
```
