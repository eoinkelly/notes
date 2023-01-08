# Inbox

http://vector.tutsplus.com/tutorials/tools-tips/an-introduction-to-illustrators-color-tools/
http://www.smashingmagazine.com/2011/01/17/productive-web-design-with-adobe-illustrator/



ctrl-shift-d = toggle transparencey grid and white bg on artboard

view
	pixel preview = see how the doc will look when rasterised at the chosen raster resolution
	outline = change to a wireframe view of the artboard

"illustrator effects" are live and can be changed on the fly
"photoshop effects" aka filters are permenant changes to document artwork - they are applied at the current document raster effects resolution and can't be changed afterwards


pixel alignment dramas

	AI can have artboards that don't have exact pixel dimensions
	should artwork be snapped to the pixel grid? what does that mean?

new document has a tickbox for align new objects to the pixel grid

need to do a

registration color = this color will be printed on all plates during the printing process

http://tv.adobe.com/watch/learn-illustrator-cs5/creating-pixelaligned-web-graphics/

Effects are "live" and can be edited on the fly (including changing their resolution); Filters are permanent changes to the artwork and can only be changed by Undoing, if you're lucky.

o the Effects menu is split into two sections: Illustrator effects, and Photoshop effects. Illustrator effects are resolution independent in that you can change the resolution at any time and the effect will look the same. Illustrator does the math for you under the hood and gives you a result you expect. However, when you use Photoshop Effects, those are resolution dependent and such effects look very different depending on what the resolution setting is. For example, the Mezzotint filter offers a very different appears when the resolution is set to 72 ppi than it does with the resolution set to 300 ppi. So you can't just change the resolution right before you print your file, because the apeparance of the effects may change dramatically. (This is why the Print dialog box smartly informs you what the Document Raster Effects Setting is set to, but it doesn't allow you to change it -- as doing so may change the appearance of your file without you knowing it. For this reason alone, if you work in print, it's best to set that resolution setting to 300 ppi at the outset of your design process.

# Checklist for setting up a illustrator file for creating web graphics
===================================================================

Creating a new document
----------------------
	size = 960px width is a good guideline (it works on all desktop screens). you can make the height whatever you want. ? height recommendation? 768-browser chrome?
	set 'units' to pixels
	color mode = rgb
	raster effects = screen 72ppi
	bleed = 0px (no such thing as bleed on the web)
	preview mode = pixel
	NB tick 'align new objects to pixel grid'

Preferences
	keyboard increment = 0.5px why?o

	units
		general = pixels
		stroke = pixels
		type = pixels
	grid
		gridline every ? px
		subdivisions ?
		show pixel grid (above 600% zoom) = on

stroking objects
	center stroke is ok as long as 'align to pixle grid' is ticked



make sure the artboards are aligned to the pixel grid or else the artwork in them will be off too

Importing a doc from unknown source
---------------------------------

File->Document color mode to RGB
File -> Document Setup
	units: pixels
	bleed: all 0px
Edit -> Assign profile -> tick don't color manage this doc
For each artboard
	edit properties to be an even num of pixels wide, high and even pixel num placement X, Y
Select all objects and choose 'align to pixel grid' from transform window
	? what happens to new objects created after this? are they aligned to pixel grid too?