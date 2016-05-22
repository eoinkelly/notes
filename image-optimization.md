# Image optimization

TODO: these notes are out date and need review

Sources

* http://www.smashingmagazine.com/2009/07/01/clever-jpeg-optimization-techniques/
* http://www.smashingmagazine.com/2009/07/15/clever-png-optimization-techniques/
	* http://www.smashingmagazine.com/2009/07/25/png-optimization-guide-more-clever-techniques/

Recommendations

* Run all images through imageoptim either manually or as a build step

## Exporting from photoshop

1. Reducing image complexity before export
	* posterization
	* blur
1. Picking the right format
1. If JPEG, choose progressive or baseline (TODO: how to choose)

## CSS sprites

TODO: are they worth bothering with in 2016?
	presume HTTP2 will impact this?


## Command line image optimization

Recommendation

* Use Imageoptim for everything on Mac

Command line image optimization tools (windows)

1. jpegtran -copy none -optimize -perfect src.jpg dest.jpg
2. optipng.exe -v -o 7 -out output.png -full input.png
3. pngcrush -rem alla -reduce -brute image.png  result.png

## Whether to specify dimensions

TODO: update this for 2016

When the browser lays out the page, it needs to be able to flow around replaceable elements such as images. It can begin to render a page even before images are downloaded, provided that it knows the dimensions to wrap non-replaceable elements around. If no dimensions are specified in the containing document, or if the dimensions specified don't match those of the actual images, the browser will require a reflow and repaint once the images are downloaded.


Be sure to set the dimensions on the <img> element itself, or a block-level parent. If the parent is not block-level, the dimensions will be ignored. Do not set dimensions on an ancestor that is not an immediate parent.


1. To prevent reflows, specify the width and height of all images, either in the HTML <img> tag, or in CSS (the <img> ‘display:’ type must be block or inline-block)
2. Don't use width and height specifications to scale images on the fly. Specify dimensions that match those of the images themselves.


Ideally don’t scale images using HTML/CSS attributes - responsive design may require a rethink of this.







## favicons

browser will always request it so it’ll 404 if missing so best to provide one and make it small
make sure it’s cacheable

If you place favicon.ico and apple-touch-icon.png in the root of your
domain and delete these references (reduced code size)
The favicon is the icon shown to the left of the URL at the top of your browser window.
The browser will always request /favicon.ico unless you specify a link in your HTML.
THe correct way to link is <link rel="shortcut icon" type=”image/x-icon”  href="/path/to/icon.ico">
Can use PNG for favicon but IE6 does not support it.
favicons should be served with mime-type 'image/x-icon' for maximum compatibility.
only specify type=”” attribute if it’s not set correctly by web-server (check .htaccess)


Favicon should be 16x16 px. They *can* be displayed at 32x32 depending on
where the user drags the shortcut on the desktop but it’s a edge use case
so I don’t think it’s worth the extra file size.


Set a long expires header on it so that it caches well
this can be done in .htaccess
I prefer to link in HTML
- extra line of HTML
+ don’t always have control of document root on server so more flexible
+ using the link allows me to version the icon and force a cache refresh
    **/


On iPhones and iPads you can book mark a web page and have it show up on the
home screen as an icon. The apple-touch-icon.png becomes this icon if used.
Similar setup to favicon.ico - browser will look for it in document root unless it’s specified in HTML. I believe this image is only asked for when the user decides to add your site to their homescreen.
Rounded corners and glossy finish are added by the device.
only specify type=”” attribute if it’s not set correctly by web-server (check .htaccess)


I prefer to explicitly link to it in HTML
- extra line of HTML
+ more felxible as I don’t always have control of document root
+ using the link allows me to version the icon and force a cache refresh
FIXME - is the icon *always* requested like favicon??


You can prevent the addition of all effects by naming your icon apple-touch-icon-precomposed.png (this is available in iOS 2 and later).
        <link rel="apple-touch-icon" href="apple-touch-icon-precomposed.png" /> <!-- no effects added -->


Different apple devices use different sizes but will scale whatever you give them. For best results supply all versions:
As of Feb 2011 the following are used:
        iPhone (≥4) — 114x114.
        iPad — 72x72.
        iPhone ≤3GS — 57x57 — If possible.


<link rel="apple-touch-icon" href="apple-touch-icon-iphone.png" />
<link rel="apple-touch-icon" sizes="72x72" href="apple-touch-icon-ipad.png" />
<link rel="apple-touch-icon" sizes="114x114" href="apple-touch-icon-iphone4.png" />


The icon that is the most appropriate size for the device is used. If no sizes attribute is set, the element’s size defaults to 57 x 57.


More info about setting a startup image and hiding safari chrome at:
http://developer.apple.com/library/safari/#documentation/appleapplications/reference/safariwebcontent/ConfiguringWebApplications/ConfiguringWebApplications.htm


## CDN & Parallel Downloading

most browsers will only download 2 files in parallel from each hostname  (TODO: is this still true in 2016?)

so try to keep static resources on different hosts. the downside of this split is more DNS lookups for the browser so keep the no. of hostnames sensible. yahoo recommends 2-4 hostnames as best balance


img data embedded in the CSS


data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAFCAYAAACJmvbYAAAAMUlEQVQIW2NkYGDwAWIQ2AKlQXwwmxFJAKYAQxIk8R+IfZFNgemEisEVoBgLk0QxAQCrgglScYuI0wAAAABJRU5ErkJggg==


## PNG Format

The PNG8 format is a palette (that is, indexed) image format, similar to the GIF format. PNG8 transparency is supported by all Grade-A browsers, including IE6. PNG8 file size is often much smaller than GIF file size.
Do not confuse the PNG8 format with truecolor PNG (also referred to as PNG24 or PNG32). The truecolor PNG format is not supported in IE6, and, if used, results in displays of gray squares instead of normal images.

There are 5 scanline filters
1. none (no filtering)
2. sub (subtract the left pixel form the current value)
3. up (subtract the above pixel value))
4. average (subtract the average of the left and upper pixels
5. paeth (substitute the upper, left or upperleft pixel value


5  ‘image type’ available
1. grayscale
2. truecolor (PNG-24 in PS)
3. indexed color (2 flavours)
1. bit transparency (each pixel is either fully opaque or fully transparent) (PNG-8)
2. palette transparency (each pixel can be semi-transparent as each colour is stored in the palette with it’s alpha value
1. grayscale w. alpha
2. truecolor w. alpha (PNG-24 w. transparency in PS)


the ones in blue are what phtoshop can save


Tools like optipng and pngcrush do the following:
Essentially, these tools do the following:
1. Pick up the best image type for an image (for example, truecolor can be converted to indexed-color if there aren’t too many colors in the image).
2. Pick up best delta filters.
3. Pick up the best compression strategy and, optionally, reduce the color depth.


## PNG Optim in Photoshop


true-color source image
1. posterize it
1. (remember that if you are using PNG-8, you can size the color palette to match your no. of levels
2. + this helps the scanline filters by reducing the num of colours in the img
3. - watch out for color alteration (ifyou are matching a graphic to a background
1. more subtle color reduction
1. open original image and ctrl-click to select it’s non-transparent areas
2. create a new channel from this selection (semi-trans areas are saved as grey)
3. open duplicate copy of full colour image w. transparency
4. remove transperncy from the copy
5. change it to indexed colour (choosing num colours, diffusion and amount of idffu)
6. convert back to full color
7. paste back into original file as new layer and allign it
8. apply the channel as a mask and then save as png24
1. if image has opaque and transparent parts, see if you can split it into 2 images so you can target compression better (the opaque image might be better as a jpeg or an 8 bit png
2. use influence masks - they tell PS which parts of the image are most important
1. Influence mask works exactly the same as regular transparency mask: white color means highest priority in corresponding image region, black color means lowest priority.
1. user dithering masks - they tell PS which parts of the image should get dithered
1. Dithering influence mask works exactly the same, but instead of colors, it affects the dithering amount of different image areas. Lighter color means more dithering. This is a very useful feature, because dithering creates irregular pixel patterns which hinders the PNG compressor to use delta filters. You can determine the exact areas where dithering must be applied while leaving other areas intact, thus gaining better compression of image data.
1. if image is grayscale
1. convert to grayscale in photoshop
2. save as true-color. png
3. use optipng -o5 image.png to convert to grayscale
1. reduce detail where it isn’t as visible
1. select only the barely opaque pixels (ctrl-click on layer, save as channel and then use ‘threshold levl on the channel to remove all but the most transparent (white) pixels
2. then apply filter->noise->median to smooth out pixels in the seleciton and make them easier for the compressor to optimise


Dithering refers to the method of simulating colors not available in the color display system of your computer. A higher dithering percentage creates the appearance of more colors and more detail in an image, but can also increase the file size. For optimal compression, use the lowest percentage of dither that provides the color detail you require. Images with primarily solid colors may work well with no dither. Images with continuous-tone color (especially color gradients) may require dithering to prevent color banding.




## Color Reduction In Photoshop

PS Color Reduction Method
Specifies a method for generating the color lookup table and the number of colors you want in the color lookup table. You can select one of the following color reduction methods:


1. Perceptual -Creates a custom color table by giving priority to colors for which the human eye has greater sensitivity.
2. Selective - Creates a color table similar to the Perceptual color table, but favoring broad areas of color and the preservation of web colors. This color table usually produces images with the greatest color integrity. Selective is the default option.
3. Adaptive - Creates a custom color table by sampling colors from the predominant spectrum in the image. For example, an image with only the colors green and blue produces a color table made primarily of greens and blues. Most images concentrate colors in particular areas of the spectrum.
4. (Restrictive) Web - Uses the standard 216‑color color table common to the Windows and Mac OS 8‑bit (256‑color) palettes. This option ensures that no browser dither is applied to colors when the image is displayed using 8‑bit color. (This palette is also called the web‑safe palette.) Using the web palette can create larger files, and is recommended only when avoiding browser dither is a high priority. we no longer use this
5. Custom - Uses a color palette that is created or modified by the user. If you open an existing GIF or PNG‑8 file, it will have a custom color palette.


choose ‘selective’ as the color dither algorithm by default
start with dither at 0% and increase the minimum amount to remove banding


Setting Transparency
PS default transparency (0-49% trans => full opaque, 50-100% trans => full trans)
By default PS will make all pixels with greater than 50% transparency fully transparent and all pixels with 50% or less transparency fully opaque unless you specify a transparency dither algorithm+amount or a matte color


Transparency Dithering
When the Transparency option is selected, you can choose a method for dithering partially transparent pixels:
1. No Transparency Dither applies no dither to partially transparent pixels in the image.
2. Diffusion Transparency Dither applies a random pattern that is usually less noticeable than Pattern dither. The dither effects are diffused across adjacent pixels. If you select this algorithm, specify a Dither percentage to control the amount of dithering that is applied to the image.
3. Pattern Transparency Dither applies a halftone-like square pattern to partially transparent pixels.
4. Noise Transparency Dither applies a random pattern similar to the Diffusion algorithm, but without diffusing the pattern across adjacent pixels. No seams appear with the Noise algorithm.


choose ‘diffusion’ as the color dither algorithm by default
start with dither at 0% and increase the minimum amount required to smooth edges (dither increases file size)




Interlace
Displays a low-resolution version of the image in a browser while the full image file is downloading. Interlacing can make downloading time seem shorter and can assure viewers that downloading is in progress. However, interlacing also increases file size.


The percieved speed increase isn’t worth the extra filesize IMHO.
Do not interlace




Web Snap
Specifies a tolerance level for shifting colors to the closest web palette equivalents (and prevent the colors from dithering in a browser). A higher value shifts more colors.


The 216 color ‘web palette’ is only relevant for monitors that don’t show many colours - all monitors now have 16+ million colours so we can safely ignore this.


leave web-snap at 0%


## JPEG Optimisation in PS

JPEG compresses on an 8x8 grid so align edges to this for best compression and sharpest edges
recommend setting your grid in PS to 8x8
also has implications for font-size choice relative to resolution of the image


it’s also possible to convert image to LAB to remove redundant colour data - this is quite fiddly and advanced I think




when you set the Quality to under 50 in Photoshop, it runs an additional optimization algorithm called color down-sampling, which averages out the color in the neighboring eight-pixel blocks:So, if the image has small, high-contrast details in the image, setting the Quality to at least 51 in Photoshop is better.


if image has small details, set quality to at least 51


Resizing & cropping in PS
to use crop tool w/o resampling make sure the resolution box is empty
if width, height & resolution are specified, PS will resample the image as your crop - it will use the default interpolation method specified in general prefernces
the f key cycles through screen modes


The ppi resolution of web images only matters if designers are specifying type in pt - if you set the doc to 72 ppi then 1 pt = 1 px and the image text prints at a reasonable size. You should use 72 ppi for web docs for convenience or change your type units to pixels.

