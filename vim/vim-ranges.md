# Ranges

Format :{range}{command}

{range} = {start},{end}

{start} can be a pattern absolute number relative number `+4` a line shortcut
(see below)

- Ranges are an `ex` thing.
- All/most `ex` commands seem to take them
- The default range is the current line except for `:g` and `:w` where it is the
  whole file (`%`)
- You can make a range by doing a selection in visual mode and then typing `:`

Line shortcuts

- `.` = current line ( you can add/subtract lines from this by appending `+` or
  `-`
- `$` = last line in file
- `'<' = start of the last (not neccessairly current) visual mode selection
- `'>' = end of last (not necessiarly current) visual mode selection

'a = position of mark a

.`5` and `.+5` both mean current line plus five lines Range shortcuts (replaces
X,Y)

5: is a shorthant for :.,.+4

- % = whole file (same as :1,$)

## Commands

Any `ex` command will work. These are some useful ones:

:t or `:co` = copy lines `:d` = delete lines `:m` = move lines

## Examples

:9t. :9copy. copy line 9 to just below current line
