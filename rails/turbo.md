The JavaScript window and document objects, and the <html> element, persist from
one rendering to the next

so what happens to events?

when you delete an element from the dom? when the elemetn still exists?

Drive

visit = location + action

Visits represent the entire navigation lifecycle from click to render. That
includes

Visit lifecycle:

1. Change browser history
1. Issue the network request
1. Restore a copy of the page from cache (if possible)
1. Render the final response
1. Update the scroll position

There are two types of visit:

1. Application visit,
    - always includes a network request
    - Turbo will try to render the page from cache immediately after visit
      starts
    - if URL includes an anchor, Turbo will scroll to it, otherwise scroll
      position is at top
    - The action determines:
        - how browser history is updated:
            - _advance_ action
                - push new entry onto browser history stack using
                  `history.pushState()`
            - _replace_ action
                - discard the top entry of browser history stack and replace it
                  vai `history.replaceState()`
    - Possible actions:
        1. advance
        2. replace
1. Restoration visit
    - happens if user uses browser back/forward buttons
    - turbo will try to render page from cache without a network request
    - turbo saves scroll position of each page before navigating away and will
      reset it when navigating back
    - actions
        - restore (for internal use only, don't set it explicitly)
    - you can control caching via a head element:

        ```html
        <!-- tell Turbo not to show a preview when doing a restoration visit to this page -->
        <meta name="turbo-cache-control" content="no-preview" />

        <!-- tell Turbo to never cache this page => always hit the network for it -->
        <meta name="turbo-cache-control" content="no-cache" />
        ```

Tasks

Form handling server should return a 303 redirect after getting a stateful form
turbo will follow the redirect and use the resulting response to update the page
without reloading If form fails and server returns 4xx or 5xx then the server
should return the response with the errors directly and not redirect

    Q: is this diff from what rails does by default?

Server can respond to a form submission with a response which contains multiple
<turbo-stream> elements lets you update multiple parts of the page without
navigating this might be exactly what we need for the focus issue

Cancel a visit call `event.preventDefault()` in `turbo:before-visit`

Do extra stuff to a page before rendering it within `turbo:before-render`
event.preventDefault() // do your stuff event.detail.resume()

Do extra stuff to a request before sending it within
`turbo:before-fetch-request` event.preventDefault() // do your stuff
event.detail.resume()

DIsable turbo on an element (link, form, ...) set data-turbo="false" attr on
element or any parent re-enable with data-turbo="true" if necessary in your
heirarchy

Force a specific page to always trigger a full reload
<meta name="turbo-visit-control" content="reload">

Where should I attach events?

Events attached to window, document, <html> will stick around between page loads

Events attached to <body> and below will???

Turbo caches <body> via cloneNode(true) so any attached event listeners are
discarded. => you need to re-attach these event listeners in turbo:load

So where should I attach listeners? Turbo docs recommend adding listeners to
document or window
