# Foreword & Introduction

# 1

Nothing new

# 2

C       change from cursor to end of line
c^      change from cursor to start of line
S       change all of current line (also: cc)
3s      delete 3 chars and go into insert mode

# 3

You can use `s` to make a more repeatable action than `xi{stuff}`

```js
// Say you want to replace `+` with ` + `
console.log('a'+foo+'b'+bar+'c'+blah);
```

Using `s` to make this a repeatable action:

```vim
" make a repeatable action with s
f+s + <ESC>
;.
;.

" using `c ` not `s`
f+c  + <ESC>
;.
;.
```

# 4

If an edit needs to be repeated, try to make it so that you can use on
keystroke to find next edit point and one keystroke to repeat the edit.

This mostly means making edits so they can be repeated with `.` and using
repeated searches (`;` or `n`,`N` etc.) to find targets

# 5

using `*` and `n` for finding and `cw` and `.` for replacing to do an
incremental find and replace

# 6

Just ; and .

# 7 Pause with brush off page

More a way to think about vim than a keyboard tip


# 8 chunking undo

He advocates leaving insert mode after each thought so that when you undo it rewinds back through your thoughts. When typing he will hit `<Esc>A` to insert a new undo point.

He also will use `<Esc>o`to create a newline from insert mode instead of just `<Cr> for the same reason.

In coding work, these pauses could come after lines or other blocks of syntax

Since moving around in insert mode using `hjkl` also creates a new "change",
you could also chunk sub sentences into thoughts that way but it is very
unwieldy - you have to achieve an actual cursor movement before vim will
recognise it as a new thing - simply using right-arrow to attempt to move at
end of line does not work.

# 9

> Note the difference between using a movement command and an object.  The
> movement command operates from here (cursor position) to where the movement
> takes us.  When using an object the whole object is operated upon, no matter
> where on the object the cursor is.  For example, compare "dw" and "daw": "dw"
> deletes from the cursor position to the start of the next word, "daw" deletes
> the word under the cursor and the space after or before it.

so text objects are much more cursor forgiving
    * movements go from current position to a new given position
    * text objects consider the cursor to be "within" some larger object

* `daw` deletes a word and the surrounding whitespace
* `daW` deletes a WORD and the surrounding whitespace
* `dib`: delete inner () block
    * also works if cursor is on the ( or )
* `diB`: delete inner {} block
    * also works if cursor is on the { or }

```
// playground
function foo(a, b, c) {
    console.log('hi there');
}
```




# 79

n           repeat search same direction
N           repeat search other direction
/<CR>       jump forward to next match of same pattern
?<CR>       jump backward to next match of same pattern

# 80

`set incsearch`

<C-r><C-w> to complete the search field with the current match

# 81

nothing new

# 82

Count no. of matches in file

```vimrc
" do a :substitute with n which replaces the matched string with the same string
" use / to repeat the last search
" :%s///gn
```

# 83

```
" search for foo and put cursor at end of match
/foo/e

" repeat last search but put cursor at end of match
//e
```
