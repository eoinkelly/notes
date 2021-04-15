CSS Positioning
==============

The 'display' property controls what *type of box* the HTML element generates.
There are 16 types of box:
    inline
    block                   makes the element 'block-level'
    list-item               makes the element 'block-level'
    inline-block
    table                   makes the element 'block-level'
    inline-table
    table-row-group
    table-header-group
    table-footer-group
    table-row
    table-column-group
    table-column
    table-cell
    table-caption
    none

each box can contain either block-level elments or inline lements but not both.
The 'position' property controls how that box is positioned on the viewport
There are 3 positioning schemes
        normal flow
            display: normal;
            display: relative;
        floated
            float: left;
            float: right;
        absolute
            display: absolute;
            display: fixed; (a subcategory of absolute)