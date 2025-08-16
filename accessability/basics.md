## Tools

- https://chrome.google.com/webstore/detail/accessibility-developer-t/fpkknkljclfencbdbgkenhalefipecmb?hl=en

## Standards

- http://www.bbc.co.uk/guidelines/futuremedia/accessibility/html/

## Books

- http://rosenfeldmedia.com/books/a-web-for-everyone/
- http://www.amazon.com/Pro-HTML5-Accessibility-Professional-Apress/dp/1430241942
- https://www.gitbook.com/book/ebay/mindpatterns/details
    - a set of HTML UI patterns for building accessible widgets

## WCAG 2.0

Native OS a11y APIs

- Microsoft Active Accessibility 2.0 with UIA Express
- MSAA with IAccessible2 1.3
- Linux Accessibility Toolkit 2.10.0 and Assistive Technology - Service Provider
  Interface 2.1
- Mac OS X Accessibility Protocol Mac OS 10.9

## Making <img> tag accessible

- The title attribute = adisory information about the element for which it is
  set.
- The alt attribute = For user agents that cannot display images, forms, or
  applets, this attribute specifies alternate text. The language of the
  alternate text is specified by the lang attribute.

Recommendations

1. always set an alt attribute
2. only set a title if the image is also a link (title appears as tooltip on
   desktop browsers)

## Appropriate use of <figure>

TODO

```html
<figure>
    <img src="/macaque.jpg" alt="Macaque in the trees" />
    <figcaption>
        A cheeky macaque, Lower Kintaganban River, Borneo. Original by
        <a href="http://www.flickr.com/photos/rclark/">Richard Clark</a>
    </figcaption>
</figure>
```

- most HTML elements have a default "role" e.g.
    - `<img>` has a role of "graphic"
    - `<main>`as a role of "main content"
        - main makes no difference in page layout but it does help
    - `<div>` and `<span>` don't have strong roles
- Accessible names
    - some HTML elements can be given "accessible names"
    - <a> tag has it's contents become it's "accessible name"
    - accessible name can come from attributes e.g. the alt attribute of images
      will become the "accessible name" of the image
    - can get accessilbe name by association with another element e.g. `<input>`
      paired with `<label>`
- State
    - HTML also communicates state
    - the `required` attribute on form field is shown visually but screen
      readers can also communicate this state
- Keyboard focus
    - works out of the box with HTML
    - `<a>` can be focused and will be followed when enter is hit
    - `<button>` can be focused and hittin enter will submit it
- The Accessibility Tree
    - Browser creates the "Accessibility tree" when it creates the DOM
- Each OS has a set of accessibility APIs
    - The browser maps HTML role, state, focus etc. to the native OS a11y APIs
        - checkboxs created in native code and HTML look the same when queried
          via the accessibility API
- ARIA lets you maniplulate the browser's accessibility tree
    - has a `role` attriburte
    - has 30+ roles in ARIA 1.1.
        - there is a role for most standard UI elements
            - slider
            - dialog
            - checkbox
            - radio button
    - ARIA lets you set an "accessible name" via `aria-label`,
      `aria-labelled-by`
    - lets you set an accessible description via `aria-described-by`
    - ARIA lets you inform the accessibility API aobut states
        - aria-checked
        - aria-expanded
        - aria-pressed
        - aria-hidden
        - aria-invalid
        - aria-current

## APIs for accessiblity testing

- http://www.tenon.io/testNow.php
    - paid but cheap
    - JSON API where you submit a URL and it does an a11y evaluation
- http://www.deque.com/products/axe/
- npm install -g a11y # hits the google a11y tools

good way to implement new components is to look to the platform a11y APIs and
pattern your new thing close to one of those so that the roles will match well

## Voiceover

- Most blind users use Safari on mac (according to blind presenter at
  https://www.youtube.com/watch?v=ok9v9-Tcq0o)
    - the experience is a bit different in Chrome
- `cmd-f5` to toggle voiceover on mac

Basic things you can do

- zoom in lots
- try using site with just keyboard
- try using site with screen reader

## JS Apps

- Screenreaders read from the top of the page
- Dynamically updating the DOM defaults to providing no audio cues!
