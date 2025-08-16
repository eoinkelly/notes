# RGBa

## Browser Support

RGBA works on all my test browsers except IE 6/7/8 i can biff ie 6/7 but ie 8 is
still important and will be into the future as it will still be the main IE
version on XP

andy clarke opinions:
http://forabeautifulweb.com/blog/about/is_css3_rgba_ready_to_rock/

- always supply a fall-back colour before the rgba one
- IE 6 & 7 don’t use the fallback color if you specifiy it longhand (using
  background-color) but they do if you use the shorthand ‘background’ or if you
  use hex code so safest practice is
  http://css-tricks.com/ie-background-rgb-bug/

it’s also possible to use IE filters to shim support into IE 6 & 7 but it’s a
mess and can make browser slow (check this) also filter is not valid css ? what
are downsides of IE filters -

http://24ways.org/2007/supersleight-transparent-png-in-ie6

ie uses aarrggbb format too! the first rule below accounts for IE 8 in standars
mode somehow...? div { background:transparent;
filter:progid:DXImageTransform.Microsoft.gradient(startColorStr=#BF6464B7,endColorStr=#BF6464B7);
-ms-filter:"progid:DXImageTransform.Microsoft.gradient(startColorstr=#BF6464B7,endColorstr=#BF6464B7)";
zoom: 1; }

general css

ie specific css div { background:transparent;
filter:progid:DXImageTransform.Microsoft.gradient(startColorStr=#BF6464B7,endColorStr=#BF6464B7);
-ms-filter:"progid:DXImageTransform.Microsoft.gradient(startColorstr=#BF6464B7,endColorstr=#BF6464B7)";
zoom: 1; }

best practice is

- supply a fallback colour before the rgba one (that IE 6 & 7 will use)
- make sure design works with solid colour

or

- use transparent png - this puts support into IE 7 (and IE 6 using png fix?)

using a fallback (first col has to be in #rrggbb or has to use ‘background’
shorthand to work in ie6/7:

article on making it work on all browsers:
http://jasonkarns.com/blog/2009/11/17/roy-g-biv-alpha/

background-color: #fd7e7e; background-color: rgba(255,0,0,0.5);

what is best practice for transparent backgrounds

    use transparent 1px png for all
    	+ works on all browsers
    	? is it harder to render for the browser than a rgba()?
    	- an extra http request
    	- a pain if we want to change transparency level later
    	- needs JS on IE 6
    only use png for browsers that require it (use modernizr to test so requires JS)
    	- requires JS
    	? modern browsers get the “simpler” rule
    	? which is harder/slower for browser to render?
    	make sure the design works w solid colour and just ignore that ie6/7 won’t get opacity (do specify fallback colour
    	+ no dud images send to browser
    	-  extra text in css to specify fall-back colour

i think the philosophy of using pngs and backfixes will just let me use designs
that don’t work very well on old browsers. i think it’s more sane to make sure
the design works “simple”

# Transparent Background BP

what is best practice for transparent backgrounds

1. use transparent 1px png for all
1.  - works on all browsers
1. ? is it harder to render for the browser than a rgba()?
1.  - an extra http request
1.  - a pain if we want to change transparency level later
1.  - needs JS on IE 6
1. only use png for browsers that require it (use modernizr to test so requires
   JS)
1.  - requires JS
1. ? modern browsers get the “simpler” rule
1. ? which is harder/slower for browser to render?
1. make sure the design works w solid colour and just ignore that ie6/7 won’t
   get opacity (do specify fallback colour
1.  - no dud images send to browser
1.  - extra text in css to specify fall-back colour

i think the philosophy of using pngs and backfixes will just let me use designs
that don’t work very well on old browsers. i think it’s more sane to make sure
the design works “simple”
