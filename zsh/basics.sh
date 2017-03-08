#!/usr/bin/env zsh

# You can invoke function by
# $ source this_file.sh
# $ show_multibyte_support # <-- notice no parens
#
function show_multibyte_support {
  if [[ -o multibyte ]]
  then
    print "This zsh supports multibyte"
  else
    print "This zsh does not support multibyte"
  fi
}

