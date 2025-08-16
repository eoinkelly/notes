# WAI_ARIA 1.0 Roles

- START HERE: Good concise list of roles:
  https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques
- Good overview:
  https://www.lullabot.com/articles/what-heck-aria-beginners-guide-aria-accessibility
- Full list = http://www.w3.org/TR/wai-aria/roles
- Spec: http://www.w3.org/TR/wai-aria/

ARIA = Accessible Rich Internet Applications

Each role falls into one of the following 4 main categories (there are other
categories):

1. abstract (not used by web authors)
1. landmark (used to identify regions of the page that are navigational
   landmarks)
1. document structure (used to organise the structure of a page)
1. widget (used for user interface components)

Roles help screen readers decide what an element **is**. For example:

> Sure it's a <ul> but is it page navigation? It's got links?

## Abstract roles

- The foundation of other roles
- Used by browsers, should not appear in your code

## Landmark roles

The WAI ARIA specification defines a set of specialised "landmark" roles. These
roles provide a method to programmatically **identify commonly found sections of
web page** content in a consistent way. Simply add a role attribute to a
container element, using the most appropriate role value for the content of the
container, for example:

    <div class="content" role="main">

Landmark roles are currently supported In JAWS version 10 screen reader, NVDA
2010.1 and VoiceOver on iPhone IOS4.

The new sectioning elements in HTML5 have some overlap with ARIA landmark roles,
but in a majority of of cases there is no equivalent for the ARIA landmark roles
in HTML5. It is suggested that where there is a similarity the ARIA roles can be
used to provide semantic identification that has a practical use now, for
example if you want to use the HTML5 nav element, add role="navigation" to it,
so supporting Assistive Technology (AT) can convey the semantic information to
users. When HTML5 elements such as nav are supported by AT, you can then remove
the role as it will no longer be required.

    <nav role="navigation">

All 8 Landmark roles:

    role="application"
    role="banner" (<header>)
    role"complementary" (<aside>)
    role="contentinfo"
    role="form"
    role="main"
    role="navigation"
    role="search"

## Document structure roles:

- provide descriptions of sections in the page role="region" (<section>)
  role="article" (<article>)

## Widget roles

https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques#States_and_properties
