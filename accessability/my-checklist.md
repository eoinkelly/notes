# Book: Web Accessibility Cookbook

1. HTML tag should have `lang` attribute with the main language of the page
    - Why:
        - How browser localises things like quotes and also hyphenation
        - The accent and intonation used when the page is read out to user
        - Helps tools like Google translate
        - SEO
    - Example
        ```html
        <html lang="en-GB">
            <!-- British english -->
        </html>
        <html lang="mi">
            <!-- Te reo maori -->
        </html>
        ```
    - Notes
        - Lookup for country codes: https://r12a.github.io/app-subtags/
2. If you have multiple languages on the page then their sections should have
   appropriate `lang` attrs set.
    - Why:
        - How browser localises things like quotes, hyphenation, how numbers are
          represented in forms
        - The accent and intonation used when the page is read out to user
        - Helps tools like Google translate
        - SEO
3. Document should have good `<title>` element
    - Don't
        - Use same title for all pages on site - user will get confused if they
          have multiple tabs open on your site
    - Why
        - SEO
        - Screen readers have specific shortcuts for reading the title
4. Don't be restrictive with viewport width
    - Example
        ```html
        <!-- good -->
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        ```
    - Why
        - Let people with low vision zoom your page
5. Use ideal head element rendering order according to
   https://csswizardry.com/ct/
    - Eoin's commentary: How big a deal is this actually?
    - Example

        ```html
        <head>
            <!-- Character encoding -->
            <meta charset="UTF-8" />

            <!-- Viewport meta tag -->
            <meta
                name="viewport"
                content="width=device-width, initial-scale=1"
            />

            <!-- CSP headers -->
            <meta
                http-equiv="Content-Security-Policy"
                content="upgrade-insecure-requests"
            />

            <!-- Page title -->
            <title>Johanna’s Toy Store</title>

            <!-- preconnect -->
            <link rel="preconnect" href="#" />

            <!-- Asynchronous JavaScript -->
            <script src="" async></script>

            <!-- CSS that includes @import -->
            <style>
                @import 'file.css';
            </style>

            <!-- Synchronous JavaScript -->
            <script src=""></script>

            <!-- Synchronous CSS -->
            <link rel="stylesheet" href="#" />

            <!-- preload -->
            <link rel="preload" href="#" />

            <!-- Deferred JavaScript -->
            <script src="" defer></script>
            <script src="" type="module"></script>

            <!-- prefetch / prerender -->
            <link rel="prefetch" href="#" />
            <link rel="prerender" href="#" />

            <!-- Everything else (meta tags, icons, open graphs, etc.) -->
            <meta name="description" content="" />
        </head>
        ```

6. Make sure you have exactly one of the banner and contentinfo landmarks
7. Make sure that only one main landmark is visible at a time
8. Don't label `<header>`, `<main>`, and `<footer>` landmarks because they are
   supposed to be unique anyway and don't need a label.
9. Wrap your navigation areas in `<nav>`
    - Structuring the links as ul>li is recommended

## ARIA roles

- Supplements HTML to make the page state (which might be dynamic) available to
  assistive technology
- used to describe elements that don't natively exist in HTML or exist but don't
  yet have full browser support.
- Add to html via `role="..."` attribute
- Many HTML elements have an implicit role
- Screen readers will often announce the role when they encounter the element
- There are 88 roles.
- Roles are
  [divided up into 6 categories](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/Roles)
    1. Document structure roles
    2. Widget roles
    3. Landmark roles
    4. Live region roles
    5. Window roles
    6. Abstract roles
        - browser use only - don't use these

## Landmarks

Landmarks have

1. An _accessible role_
    - each landmark has an associated role
2. An _accessible name_
    - All **interactive** elements must have an accessible name
    - some elements get this from their content e.g. `<button>`, `<td>`, `<a>`
      use their inner text to set the accessible name
    - other elements get their accessible name from the content of associated
      elements e.g.
        ```
        <textarea> gets accessible name from <label>
        <fieldset> gets accessible name from <legend>
        <table> gets accessible name from <caption>
        ```
    - not all roles allow you to set an accessible name e.g. TODO

Landmarks created implicitly by using the correct HTML element (when it exists)
or explicitly by setting the `role` attribute

> Use these sparingly. Too many landmark roles create "noise" in screen readers,
> making it difficult to understand the overall layout of the page.

### Landmark roles

There are 8 landmark roles

1.  banner `<header>`
    - contains mostly site-oriented rather than page specific content
    - should only be one per page
    - created implicitly by `<header>` tag or explicitly by
      `<div role="banner">`
    - `<header>` creates banner landmark unless it is nested inside an
      `<article>`, `<aside>`, `<main>`, `<nav>`, `<section>`
        - Implies you can have multiple `<header>` per page as long as they are
          nested within the tags above
1.  navigation `<nav>`
1.  main `<main>`
    - contains page's core content
    - should only be one **visible** per page at any one time
    - created implicitly by `<main>` tag
1.  region `<section>`
1.  form `<form>`
1.  search `<search>`
1.  complementary `<aside>`
1.  contentinfo `<footer>`
    - there should only be one contentinfo landmark per page
    - created implicitly by `<footer>` tag or explicitly by
      `<div role="contentinfo">
    - similar to `<header>`, the `<footer>` tag only creates the landmark if it
      is outside an `<article>`, `<aside>`, `<main>`, `<nav>`, `<section>`
        - so you can use multiple `<footer>` tags as long as they are nested
          within one of the tags above

Landmarks should have a name that assistive tools can use

The name can be set by an associated element e.g. `<label>` element or via
attributes.

Attributes which can set the name of an element:

- `aria-label`
    - defines a label
    - overridden by `aria-labeled-by` if both are set
    - same purpose as `aria-labelledby`
- `aria-labelledby`
    - same purpose as `aria-label`
    - The aria-labelledby property enables authors to reference other elements
      on the page to define an accessible name.
    - is the highest precedence way to set an accessible name - overrides all
      other methods of setting a name
    - references the DOM `id` of an element
    - if you can't find suitable content to reference use `aria-label`
    - you can give it a space separated list of DOM ids and it will combine
      their contents
    - Examples

        ```html
        <span
            role="checkbox"
            aria-checked="false"
            tabindex="0"
            aria-labelledby="tac"
        ></span>
        <span id="tac">I agree to the Terms and Conditions.</span>

        <!-- combining content from multiple elements -->
        <h2 id="attr" class="article-title">
            13 ARIA attributes you need to know
        </h2>
        <p>
            There are over 50 ARIA states and properties, but 13 of them stand
            out…
            <a href="13.html" id="rm13" aria-labelledby="rm13 attr"
                >read more</a
            >
        </p>
        ```

- `alt`
- `title`
    - use `aria-label` instead for accessibility purposes
- `aria-description`
    - **It is not a substitute for `aria-label` or `title`!**
    - A string which describes the current element
    - More:
      https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/Attributes/aria-description
    - Intended to be longer and more detailed than `aria-label`
- `aria-describedby`
    - aria-describedby attribute lists the ids of the elements that describe the
      object. It is used to establish a relationship between widgets or groups
      and the text that describes them
    - Example
        ```html
        <button aria-describedby="trash-desc">Move to trash</button>
        <p id="trash-desc">
            Items in the trash will be permanently removed after 30 days.
        </p>
        ```

Screen readers can:

- list all landmarks on a page
- jump to specific landmarks directly
- announce the landmarks on a page or section when entering it

## Questions

What things should get aria-label? too many labels too noisy?

what case to use in attr values? sentence, all lower, does it matter?
