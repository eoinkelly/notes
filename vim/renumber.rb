
# input: the currently selected lines in the vim buffer
# output: a new set of lines with the numbers fixed


# goal 1: take in the buffer, make some trivial change and put it back

b = VIM::Buffer.current # also @curbuf

b.append(0, "# Be as careful in the beginning")
b.append(b.length, "# …as you are in the end.")

stuff = VIM::command('normal gv"xy')

# function! Test()
#    "yank current visual selection to reg x
#    normal gv"xy
#    "put new string value in reg x
#    " would do your processing here in actual script
#    let @x = @x . 'more'
#    "re-select area and delete
#    normal gvd
#    "paste new string value back in
#    normal "xp
# endfunction

# :rubydo $_ = $_.gsub!(...
# in rubydo the magic global $_ is set to each visually selected line one after the other
#
# def h(n)
#    win = VIM::Window.current
#    win.height = n
# end
#

# invoke the above function by
#   :rubyfile ./script.rb
# * you cannot just use filename
#
# same as
# :ruby load 'script.rb'
# # :ruby h(99)
#
#
# # the VIM module provides the interface to vim in this script
#
# # :ruby VIM::message($curbuf[3]) # show 3rd line of current buffer
