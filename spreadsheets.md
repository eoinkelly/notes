# Spreadsheets

## Ranges

There are two types of range

1. Adjacent/Symmetrical
2. Non-adjacent/Irregular

TODO: are there others?

### Adjacent/Symmetrical range

```
{top-left-cell}:{bottom-right-cell}
A1:C12
A:A # sum the whole A column
A1:A10 # Includes A1 -> A10
```

- omit the row number from both parts and Excel will consider it the entire
  column
- this range is "rectangular"
- In Excel, the minimum and maximum value are included.
- Eventually separate cells can be added to this selection, then the range is
  called an irregular cell range.
    - Q: how is this diff to the "Non-adjacent range"?

> In Excel, a range is defined by the reference of the upper left cell (minimum
> value) of the range and the reference of the lower right cell (maximum value)
> of the range

### Non-adjacent range

Seems to either use `+` or `;` as separator (both seem to work interchangeably)

```
A1+A3+B5 # includes the cells A1, A3, B5 only
A1;A3;B5 # includes the cells A1, A3, B5 only
```

## Pivot tables

- a way of summarising data

1. Choose rows
1. Choose columns
1. Choose values
1. Choose filters
    - Filtering allows you to remove rows from the **data set** (not the pivot
      table itself)

They are all about taking "columns" from your dataset: some columns are
summarised as rows in the pivot table some columns are transposed to be column
headings in the pivot table

Some columns in the data-set can become rows in your table when you add a "row"
the data-set column will be "summarised" unique values will be found if you add
multiple rows you get a "tree like" relationship "Add row" = "take a column from
the data-set and do a "group by" on it i.e. summarise it based on some criteria

"Add column" = "take a column from the dataset and use its values as column
headers in the pivot table you can take a column form your dataset and transpose
it to be column headings in your pivot table you can tweak the sort order of the
column headings in your pivot table

"filter" = remove some rows from the dataset based on some criteria the row is
removed from _all_ columns - it is as if the row wasn't in the dataset

If you use a data-set column as row you cannot also use it as a column
