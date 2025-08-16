# CSS Pixels

Sources

- http://www.quirksmode.org/blog/archives/2010/04/a_pixel_is_not.html
- http://alistapart.com/article/a-pixel-identity-crisis
- https://www.w3.org/TR/CSS2/syndata.html#length-units

- A screen has real physical pixels.
    - These are called _device pixels_ or _hardware pixels_.
    - It is the smallest point the display can show

- The CSS "pixel (the `px` unit) is defined as an optically consistent unit
    - it is designed to look the same size in all viewing situations because it
      takes viewing distance into account
    - also called a "reference pixel"

> The reference pixel is the visual angle of one pixel on a device with a pixel
> density of 96dpi and a distance from the reader of an arm's length. For a
> nominal arm's length of 28 inches, the visual angle is therefore about 0.0213
> degrees. For reading at arm's length, 1px thus corresponds to about 0.26 mm
> (1/96 inch).

- `device pixel ratio` is the ratio of reference pixels to hardware pixels
    - e.g. iPhone has DPR of 2 so each refernce pixel is two hardware pixels

Key points

- 96 "CSS reference pixels" = 1 "CSS inch"
- The CSS pixel is a "reference" pixel, not a device pixel.
- A CSS inch is exactly or 'close' to an inch.
    - usually a "CSS inch" will be exactly 1in but there may be some error on
      lower resolution devices

e.g. iPhone screen with 320 x 480 px (css pixels) which is 3.33 x 5 CSS inches

http://www.canbike.org/CSSpixels/ is a useful reference of hardware pixels and
"device width" i.e. the width you get of the screen if your page has
`<meta name="viewport" content="width=device-width">`

iPhone seems to pick 980px as a default width to render a web page into if you
odn't provide any meta tags
