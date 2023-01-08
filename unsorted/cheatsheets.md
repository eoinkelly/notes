

# Vim

Folding FIXME make descriptions more accurate
zf		create fold
zo		open fold at current line
zO		open all folds at current line
zc		close fold at cursor line
zC		close all folds t cursor line
zd		delete fold at cursor line
zD		delete all folds at cursor line
zr		reduce folding by 1 level
zR		open all folds
zm		increase folding by 1 level
zM		close all folds

zn		disable folding on current file
zN		enable folding on current file
zi		toggle folding on current file (toggles zn/zN)

:mkview		save folds

General
	,skel		load my skeleton HTML page

Autocomplete
	ctrl-x,ctrl-o	open autocomplete
	ctrl-x <space>	make the most recent word a tag pair
	ctrl-x <cr>		make the most recent word a tag pair (opening & closing tags on new lines)
	ctrl-x /		close most recently opened tag

General
	@:		repeat last command-line (can multiply by preceding with <count>)
	.=		repeat last editing operation
	:r		read named file into current buffer at cursor
	V		visually select current line
	v		visually select character

Macros
	qX		start recording into register X
	q		stop recording current macro
	@X		play macro X
	@@		replay last played macro

Buffer plugin (cant emember name)
	ctrl-tab		go to next buffer in list
	ctrl-shft-tab	go to previous buffer inlist
	alt-[0-9]		go to buffer [0-9]

Window Management
	ctrl-w |		maximise split window vertically
	ctrl-w -		maximise split window horizontally
	ctrl-w =		equalise split windows


Changelist
	:changes		print the change list
	g;				go forward in the changelist
	g,				go backwards in the changelist

Jumplist
	:jumps			print the jump list
	ctrl-o			go backwards through jumplist
	ctrl-i			go forwards through jumplist

Custom shortcuts
	F8		run tabularize
	F5		run cleanse whitespace function (FIXME this is bit broken)
	,h		toggle highlight search on/off
	,x		toggle nerd-tree window open/closed
	,x		toggle spell check underlining


Command Window
	q/		open the command window with searches
	q:		open the command window with commands
	ctrl-f	open currently being typed command in the command window
	ctrl-c	(choose) close the command window and load the currently highlighted line into the command line
	:q		close the command line window

Renumber
	:Renumber			renumber the selected lines
	:Renumber a			renumber the selected lines, search the whole line for a number, not just the selected colmns
	:Renumber d			renumber days of the week
	:Renumber m			renumber months of the year
	:Renumber r			renumber the selected lines in reverse order (start at bottom of block)
	:Renumber sX		renumber the selected lines using X as the increment

# Photoshop
	m: rectangular marquee tool

# SEO
	get stuff out of the google pdf and my blumind
