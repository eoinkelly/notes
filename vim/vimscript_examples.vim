
" run :so % to source the current file before playing with these examples

" function names are capitalized in vimscript
" `function` and `endfunction` open and close a function
function DoStuff()
  echo "i did stuff"
  return "some stirng" " returns are optional
endfunction

" invoke a function for side-effects with `call` - it discards return values
call DoStuff()
" invoke a function for return value with `echo`
echo DoStuff()


function ShowDefaultReturnValue()
endfunction

echo ShowDefaultReturnValue()
" returns 0
"

function ExampleWithArg(foo)
  " function local variables are declared with let
  let someOtherVal = 33
  " references to function argument variables are prefixed with a:
  if a:foo < 10
    return "foo is big"
  else
    return "foo is small"
  endif
endfunction

echo ExampleWithArg(11)

"
" In vim
"
"   1 is truthy
"   0 is falsy
"
" and vim has reasonably surprising way of coercing strings into truthy/falsy
"
"   "hey" coerces to number 0 which is falsy
"   "8 days" coerces to number 8 which is truthy
"
" because character strings usually coerce to falsy except if they begin with
" a digit then they coerce to to that number

" execute in vimscirpt is a bit like eval
" it concatenates all its args with a space and runs the resulting string as a
" vim command
function DemoExecute()
  execute "do" "thing"
endfunction

call DemoExecute()

