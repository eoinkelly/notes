# Vim/Vintage Training

http://ontwik.com/tools/vim-from-novice-to-professional-by-derek-wyatt-p1/

super = windows key

}k = go to last char of last line of current paragraph {j = go to first char of
first line in current paragraph % = jumps to the closing brace within a set of
braces and then toggles to the start brace works on {}()[] I = enter insert mode
and move to first non-whitespace char on line

Kata: Select everything between the p tags not incl. the tags Kata: Select
everything between the p tags incl. the tags

<p>
	Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod
	tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
	quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
	consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
	cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
	proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
	hint: vit, vat
</p>

Kata: jump from one { to } Kata: jump from one ( to )

Kata: jump from one " to " incl. "" Kata: jump from one " to " not incl. ""
Kata: jump from one ' to ' incl '' Kata: jump from one ' to ' not incl ''

Kata: select everything between "these double quotes" Kata: select everything
between 'these single quotes' Kata: select everything between the brackets not.
incl brackets Kata: select everything between the brackets incl. brackets Kata:
select everything between the braces not. incl braces Kata: select everything
between the braces incl. braces

"this is a test"

function foo (blah) { // test // Use % to jump from { to } and vice versa var x
= [ "this is a foo test", 'hello']; // vi{ = select everything between the
braces // va{ = select everything between incl. the braces themselves }
