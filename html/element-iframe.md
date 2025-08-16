# <iframe>

??? what mouse and keyboard events does JS in the frame get?

## Overview

- stands for _inline frame_
- embed one HTML document into another
- represents a nested _browsing context_
- each browsing context
    - has its own session history
    - has its own `document` (so uses more memory than the equivalent set of
      markup in the parent `document`)
    - `Window` referes to the the topmost browsing context
    - held as an `HTMLIFrameElement` in the parent's `document`
- Always add a `title` attribute so assisitive technologies (AT) can tell the
  user what's in the frame without them having to navigate to it
- Implicit ARIA role: none
- Permitted ARIA roles
    - application
    - document
    - img
    - none
    - presentation
- Content within the iframe tag is only displayed if browser doesn't support
  iframes

## Downsides

- "breaks" the back button from pov of users
- you must trust the external content you are loading because it will look like
  part of your web page
- SEO
    - Google doesn't index content in a frame (is this defn true in 2020?)
    - Searchbots consider the iframe content to be from another site so you
      don't get any credit for it
- you code running within the iframe needs to trust the page that's embedding it
  too
- browsers delay the rendering of iframes behind other things on the page
- JS communication between parent and frame is bit cumbersome
- they are fixed size on the page and cannot resize based on the content of the
  frame
- https://github.com/krakenjs/zoid
    - Paypal JS lib which tries to work around the limitations of iframes
    - Aims to use them for a sort of "micro front-end" thing

## Attributes

```
<iframe
    src="path/to/file.html"
    frameborder="0" // remove ugly default border
    title="blah" // described the contained content for screen readers
    width="300" //default ???
    height="300" // default 150
    name="blah"
    csp="" // enforce a CSP for the embedded resource
    loading="eager" // how should browser load the iframe, values: eager|lazy, eager is default,

    // set the feature policy for the frame (it's set in a HTTP header for the topmost document)
    // see https://developer.mozilla.org/en-US/docs/Web/HTTP/Feature_Policy/Using_Feature_Policy
    // setting this on iframes is supported on all modern browsers (safari doesn't support HTTP header yet)
    allow="'src'" // default allowlist
    allow="fullscreen payment"

    // these legacy attributes are replaced and expanded by allow="..." now
    // allowfullscreen
    // allowpaymentrequest

    referrerpolicy // which referrer to send when fetching the frames src

    // apply extra restrictions to the content in the frame - many possible values
    sandbox="allow-forms allow-modals allow-popups"

    srcdoc

    // there are a few deprecated attributes e.g. marginwidth
></iframe>
```

## CSS

- default styling includes a kind of sunken border
- iframes are replaced elements
    - => can be adjusted with
        - `object-position`
        - `object-fit`

## JS

- access between the documents is subject to the same-origin policy (so if the
  iframe is loaded from a diff domain then it doesn't get any extra access just
  because it is an iframe)
- cross origin communication can happen via `Window.postMessage()`

In parent

```js
window.frames; // pseudo-array of all iframes on page

frame = document.getElementById('myframe');
frame.contentWindow;
frame.contentDocument;
frame.contentDocument == frame.contentWindow.document; // true, but the comparison fails due to cross-origin
```

In child frame:

```js
window.parent; // reference to the parent Window object
```
