# Text zoom

https://www.matuzo.at/blog/2023/how-browsers-zoom-text is interesting

## font-size

Browsers which treat `px` and `rem` the same when you zoom text:

- iOS Safari
- Desktop Safari
- Desktop Firefox

Chrome (desktop or Android) does not but that looks set to change in 2024 - see
https://issues.chromium.org/issues/40484405

Soon it won't really matter which units you use to specify you `font-size`.
However, since Chrome doesn't behave like the others yet, using `rem` seems a
bit better

### Question: Any anti-aliasing issues with fractional font sizes?

Open question: If I want to hit a `font-size: 13px` with `rem` I can do

```scss
font-size: 0.8125rem;
// or
font-size: calc(13px / 16) rem;
```

Does this lead to anti-aliasing issues when the browser tries to hit a font size
that's

## everything else: width, padding, border-width, margin etc.

Browsers do not treat px and rem the same when they are used for anything else
e.g. padding, border-width, margin, width etc.

## Supporting multiple zoom levels

I don't think setting and enforcing a default is the best way to achieve this
goal.

I think if we want to make our work support text zoom we need:

1. Define how many levels of zoom we will support for the feature/project. I'm
   guessing we don't need to care too much about making text smaller so maybe
   default level plus two zoom-in's (3 zoom levels overall?)
2. Define what "support" means for the project/feature. Does it just me "not
   totally borken" or does it mean "looks great"
3. Test the zoom levels like we do any other accessibility testing

If those are in place then choosing the most appropriate CSS dimensions falls
out of that.

Sometimes we will want dimensions to scale with text but many times we will
not - I think it'll still be most common to want dimensions specified as a
fraction of the element container or viewport. I can imagine times where the
dimension not scaling with font would be a feature.

## Question: how much does each zoom level increase the size?

It's currently explicit in chrome if you go Settings > Appearance > Customise
fonts but on other browsers it's hard to tell
