# Overview

TL;DR the barrier to `<picture>` is being able to re-encode JPEGs and PNGs as
WEBP (or whatever). It's pretty easy for images added by a developer but images
uploaded by user is just fiddly/expensive enough for it to not be feasible for
most projects.

## Responsive image options

* Approach 1: as responsive as possible with a single image file
* Approach 2: multiple image files, browser choses best one
    * options
        1. use `<picture>` with multiple `<source>` tags and a fallback `<img>` tag
        2. use `<img>` with `sizes` and `srcset`
    * challenges
        * need a pipeline that can transcode images
        * both images added by devs (easy enough) and images uploaded by users (harder)

## <img>

* width and height attrs will be used by browser to calculate an aspect ratio for the width and height set in CSS
    * best practice is to add `width` and `height` attributes to the `img`
    * must also set `height: auto` in the CSS

```html
<style>
img {
  width: 100%;
  height: auto; // you
}
</style>

<img src="..." width="100" height="100" />
```

## <picture>

Sources

* http://www.html5rocks.com/en/tutorials/responsive/picture-element/

Browser support and polyfill

* 2020: excellent browser support
* `<picture>` is designed to solve the "art direction" case where you want to load entirely different images at different sizes
* `srcset` is designed for loading different versions of the same image at differerent pixel densities

## <img srcset="" sizes=""

> The srcset and sizes attributes on img (or source) elements allow authors to
> define various image resources and "hints" that assist a user agent to
> determine the most appropriate image source to display (e.g. high-resolution
> displays, small monitors, etc).

```html
<img sizes="(max-width: 30em) 100vw,
            (max-width: 50em) 50vw,
            calc(33vw - 100px)"
    srcset="swing-200.jpg 200w,
            swing-400.jpg 400w,
            swing-800.jpg 800w,
            swing-1600.jpg 1600w"
    src="swing-400.jpg"
    alt="Kettlebell Swing">
```

TODO: what does "sizes" do?
	seems to hint to browser which image to use at which screen res but leave the choice to the browser???

## Newer Image formats

Possible replacements for JPEG and PNG

1. WEBP
    * 2020-09-10: good browser support https://caniuse.com/?search=webp
2. HEIC/HEIF
    * 2020-09-10: no browser support https://caniuse.com/?search=heic
    * Conclusion: ignore for now
3. AVIF
    * 2020-09-10: basically no browser support https://caniuse.com/?search=avif
    * Conclusion: ignore for now
4. JPEG2000
    * only Safari supports it
    * seems to be going nowhere
5. JPEG XR
    * Only IE11 support it
    * seems to be going nowhere

