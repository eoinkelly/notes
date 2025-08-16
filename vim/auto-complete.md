# Vim Autocomplete

- Autocomplete is triggered from insert mode

There are many types of autocomplete in vim and each is opened with a different
key chord

    C-n         generic keyword autocomplete (includes buffer list, included files, tag files)
    C-p         generic keyword autocomplete (includes buffer list, included files, tag files)
    C-x C-n     current buffer keywords
    C-x C-i     included files keywords
    C-x C-]     tags file keywords
    C-x C-k     dictionarylookup
    C-x C-l     whole line autocomplete (uses same list of sources as generic keyword autocomplete)
    C-x C-f     filename completion
    C-x C-o     omni completion (vim's version of intellisense)
    C-x C-s     show spellcheck autocomplete (requires :set spell)

```ruby
# Use C-x C-f repeatedly to navigate through a file heirarchy
# When popup is open use C-p to allow you to filter as you type
require_relative "../ruby/bikes/lib/bike"
```

Navigating the popup

    C-n next entry
    C-p previous entry
    C-h delete one letter

    C-c close buffer (accepts current entry)
    C-y close popup, resets text to what it was before popup
    Typing any other character exits the popup and puts that char in the buffer

Great Tip: Type C-n C-p together to make the autocomplete list filter as you
type
