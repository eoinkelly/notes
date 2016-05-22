# picture element & srcset, sizes attributes

Conclusion (May 2016)

* I still class picture element as a perf optimization
* it is sufficiently complicated to not be part of default workflow
* browser support is uneven, polyfills are good but have some unavoidable caveats
* couldn't find examples of large sites using it

Sources

* http://www.html5rocks.com/en/tutorials/responsive/picture-element/

Browser support and polyfill

* http://caniuse.com/#feat=picture
    * Good support in firefox,chrome, ie-edge
    * only supported in the very latest browsers for everything else
* picture element has slightly better browser support than srcset - see http://caniuse.com/#feat=srcset

* has a good polyfill
    * http://scottjehl.github.io/picturefill/
        * https://github.com/scottjehl/picturefill
    * this polyfill is a `srcset` polyfill too!
    * you can use modernizr (or write your own test) to only include the polyfill if it is needed
* a second polyfill
	* https://github.com/aFarkas/respimage

* `srcset` is designed for loading different versions of the same image at differerent pixel densities
* `<picture>` is designed to solve the "art direction" case where you want to load entirely different images at different sizes

Real world usage

At 25 feb 2016, smashing mag tweeted some polls asking about usage in production

* 42% of respondants said they use responsive images and of that 42%, 39% said they used a polyfill
	* i.e. 16.3% of respondants said they use responsive images with a polyfill
	* I think I can ignore the "non polyfill" people as being sufficiently different use-case to mine
* tweets
	* https://twitter.com/smashingmag/status/702469919817011200
	* https://twitter.com/smashingmag/status/702469930348900352

## srcset

```
<img sizes="(max-width: 30em) 100vw,
            (max-width: 50em) 50vw,
            calc(33vw - 100px)"
    srcset="swing-200.jpg 200w,
            swing-400.jpg 400w,
            swing-800.jpg 800w,
            swing-1600.jpg 1600w"
    src="swing-400.jpg" alt="Kettlebell Swing"><Paste>
```

TODO: what does "sizes" do?
	seems to hint to browser which image to use at which screen res but leave the choice to the browser???


# when to use srcset vs picture

* http://blog.cloudfour.com/dont-use-picture-most-of-the-time/

