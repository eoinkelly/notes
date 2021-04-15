Base styles
	the defaults we set
	almost exclusively single element selectors
		could include attribute selectors, pseudo-class selecotrs, child selecotrs, sibling selectors
		does not include class or ID selectors - it's default styling for how that element should look in all occurances on the page.
	"wherever this element is on the page, it should look like this"

CSS shouldn't rely on the HTML structure of the page
Modules
	reusable, modular parts of our design

Layouts
	divide the page into sections, they hold all your modules
	these are the major componenants on the page (the minor components are modules

State styles
	ways to describe how modules & layouts will look when in a particular state
		hidden or expanded
		active or inactive

Naming convention
on large projects, css is split across files
l- = layout
s- = state
modeules are the bulk of a project so starting with .mod is needlessly verbose
	he does stick to a 3 letter prefix for modules
	.exm
	.cll
	.fld

	these are used as the name-stem for related styles
	.exm (example)
	.exm-caption 
