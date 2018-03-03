# Grid layout

Sources:

* https://css-tricks.com/snippets/css/complete-guide-grid/
* Wes bos video series https://courses.wesbos.com/

Basics

* A new CSS specification
* is being worked on by browsers but not really available anywhere right now
* when the day comes that grid layout is supported by all browsers in your "browser support" matrix you should absolutely use it.
* columns and rows are collectively known as "tracks"
* you set `display: grid` on the container and all its children become grid items
    * ? is it only immediate children?
* the `fr` unit
    * often replaces the need for % units in grid
    * represents the amount of space left over after all the explicit elements have been laid out
    * fr is better than % becuase it accounts for any `grid-gap` you have so you don't have to do math e.g. `grid-template-columns: 1fr 1fr 1fr` will make three even columns which take up the full width of the grid no matter how much grid-gap exists
    * fr behaves like % in that it is responsive to screen size changes
* `auto` keyword
    * makes the row or column adjust to the contents of it
* the `repeat(n, thing)` function is defined as part of the grid layout spec
    * QUESTION: does it only work in grid?
* `auto-fill`
    * used as a count argument within the `repeat(count, width)` function
    * tells browser to make as many columns as will fit within the container
    * behaves the same as `auto-fit` provided there are enough columns to fill one row
    * when there are not enough content items to fill one row of grid
        * auto-fill extends the explicit grid to the width of the container leaving empty grid cells
* `auto-fit` keyword
    * used as a count argument within the `repeat(count, width)` function
    * behaves the same as `auto-fill` provided there are enough columns to fill one row
    * when there are not enough content items to fill one row of grid
        * auto-fit ends the explicit grid with the last content item even if other grid cells would fit in the container width
    * tells browser to make as many columns as will fit in the width of the container
* `minmax(min_width, max_width)`
    * a function
    * can be used in `grid-template-columns` and `grid-template-rows`
    * the min or max can be `1fr` aka the full width of the container
* `fit-content(max_value)`
    * use it as a magical width in `grid-template-columns|grid-template-rows`
    * it makes the column be sized by its content up to whatever `max_value` you specify

Terminology

* Line
    * one edge of the bounding box around a cell
    * lines exist between area and column
    * lines are 0 width by default but can be given width with `grid-gap`
    * each line can have 0-many names
* Cell
    * a single "unit" of grid
    * The space between two **adjacent** row lines **and** two **adjacent** column lines.
* Area
    * The total space surrounded by four grid lines i.e. one or more complete rows and columsn of cells
    * A grid has many areas
        * Each cell is an area (smallest area)
        * The grid itself is an area (largest area)
        * There may be many areas in between
* Track
    * The space between two grid lines (i.e. either a row or a column)
    * Is a particular kind of grid area which contains only all of one row or column


Firefox Grid inspector

* solid line => the explicit grid starts or ends (i.e. the outer edge)
    * there may be implicit columns or rows **outside** the explicit grid
* dark dashed line => a grid line we explicitly created
* dotted line => a grid line implicitly created for us by browser

```scss
.container {
    // * Make the container a grid (which makes its children grid items)
    // * grid-items are still `display: block` - they do not have a different
    //   display property, only the container does
    // * Only the **direct** children of a grid container are treated as grid items
    display: grid;

    // * Explicitly define some columns
    // * `auto` does what you would expect (you can use it more than once)
    // * supports the repeat(n, str_to_repeat) function
    grid-template-columns: 1fr 1fr auto 1fr;
    grid-template-columns: 1fr auto auto 1fr;
    grid-template-columns: repeat(4, 50px);

    // You can name lines and refer to them by name instead of number when placing grid items
    // A line can have multiple names (separated by whitespae)
    grid-template-columns:
        [adverts-bar-start] 1fr
        [adverts-bar-end nav-bar-start] 1fr
        [nav-bar-end main-content-start] auto
        [main-content-end sponsor-bar-start] 1fr [sponsor-bar-end];



    grid-template-columns: repeat(auto-fit, 50px);
    grid-template-columns: repeat(auto-fill, 50px);

    // each column is between 50px and the full width of the container (natural
    // responsiveness)
    // auto-fit and minmax give you a good horizontal menu layout
    grid-template-columns: repeat(auto-fit, minmax(50px, 1fr);


    // make the first column be sized by its content up to a maxiumum of 150px
    grid-template-columns: fit-content(150px) 150px 150px 150px

    // * create explicit grid rows
    // * supports the repeat(n, str_to_repeat) function
    grid-template-rows: 50px 50px 50px 50px; // create 4 explicit rows of height 50px
    grid-template-rows: repeat(4, 50px);     // same as above
    grid-template-rows: 50px 100px 66px;    // create three explicit rows of different heights

    // row (default) => if you have more grid-items than will fit in the explicit grid then it will create new implicit rows for them
    // column => if you have more grid-items than will fit in the explicit grid then it will create new implicit columns for them
    // dense => if you have grid items which span more columsn than the browser
    //          has available, it has to move the spanning item to the next row or column.
    //          Normally this would create gaps but with 'dense' the browser will fill in
    //          those gaps with the available smaller items. This means the browser
    //          re-orders your grid items. You can only use this if you don't care about the order.
    grid-auto-flow: column; /* row|column|dense */

    /*
    Set the size of any implicit rows created by browser
    Firefox can only take one value, chrome can take multiple values (bug in firefox)
    */
    grid-auto-rows: 400px;
    /*
        * Setup a gap between grid items
    */
    grid-gap: 20px;

    grid-column: span 2; // span 2 cells
    grid-row: span 2; // span 2 cells

}

.some-item {
    // these numbers are **track numbers** NOT column numbers!
    grid-column-start: 2; // start this item at track 2
    grid-column-end: 6; // end this item at track 6
    grid-column: 2 / 6; // same as both lines above together
    grid-column: 2 / span 4; // same as line above

    // or

    grid-column: 1 / -1 // span  all tracks (where num tracks is unknown)
    grid-row: 1 / -1 // only goes to the bottom of the **explicit** grid

    grid-column: span 3 / 9 // will anchor the end of this item at track 9 and then span 3 back from there

}

```

### grid-template-areas

* lets you name cells in the grid
* multiple cells can have the same name - in that case any content which targets them does an implicit span
* you can redefine `grid-template-areas` propery in media queries to completely rearrange your layout at different page sizes

```
.container {
    display: grid;
    grid-gap: 20px;
    border: 10px solid var(--yellow);
    grid-template-columns: 1fr 500px 1fr;
    grid-template-rows: 1fr 1fr 100px;

    /* this property can be redefined in a media query so you can move things around in the grid easily */
    grid-template-areas:
    "left-sidebar content right-sidebar"
    "left-sidebar content right-sidebar"
    "footer footer footer"
}

.item4 {
    /* now we can used the grid template areas we have defined as targets for placement */
    grid-area: footer; /* does not work as a quoted string */
}
.item5 {
    // The names you create in `grid-template-areas` implicitly create lines
    // for you - you can access these lines by adding `-start` and `-end` to
    // the name of the grid area. You can use these lines instead of row and
    // column numbers for placement
    grid-column: footer-start / footer-end;
}

```

### Aligining items

* grid can be used for both horizontal and vertical centering

* there are 6 relevant properties
* `justify-*` are along the row axis
* `align-*` are along the colum axis
* unlike flexbox they don't swtich around

```scss
// applied on the container, effects **width** of items within the container
justify-items: stretch; // stretch (default) |center|start|end

// applied on the container, effects **height** of items within the container
align-items: stretch; // stretch(default)|center|start|end

// shorthand that sets both justify-items and align-items (WARNING: is newer, might not have browser support)
place-items: JUSTIFY_ITEMS_VALUE ALIGN__ITEMS_VALUE
// examples
place-items: center center; // center both ways
place-items: start end; // put item in bottom left corner of the grid item

// does the same as *-items except it is applied to an item not the container and it targets that item
justify-self: stretch; // stretch(default)|center|start|end
align-self: stretch; // stretch(default)|center|start|end

// *-content controls the grid itself within its container i.e. what if your grid does not fill its container
// it lets you control where to place the grid itself within its container
// it essentially dynamically sizes 'grid-gap' to achive what you want
justify-content: stretch; // stretch|start|end|center|space-around|space-between
align-content: stretch; // stretch|start|end|center|space-around|space-between
```

### Ordering

* you can use the `order` property on a grid item to control its order relative to other items
* default order of everything is 0 so any number higher than that will be pushed to the end
* to meaningfully use it you have to add `order` to every item to control it properly
* it can be handy to change this property in media queries
* be aware that screenreaders will not use this new order and that it makes selecting text across the items with your mouse a bit odd

```
order: 1 // default is 0, can be any number
```

