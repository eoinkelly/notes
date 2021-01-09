# HTML 5 Elements

* Handy: http://html5doctor.com/
* use the 'sections' col from: http://html5doctor.com/
* It's better to use a div rather than use the wrong new element in the wrong place

The 4 sectioning elements are:

1. article
2. aside
3. nav
4. section

## Elements

* div
	* has no special meaning
	* can be used with class, id, lang attributes to group it's children
	* it is a container for "flow content" (HTML5 flow content ~= HTML4 block level elements)

* body
	* represents the main content of a document

* blockquote
	* represents content that is quoted from another source
	* the source's address (if it has one) may be cited in the "cite" attribute.

* header
	* represents the header of a document or a section of a document
	* represents a group of introductory or navigational aids
	* the document as a whole has a <header> but so can each section (as defined by <article>, <nav>, <aside>, <section>)
	* typically used to group h1-h6 tags but can contain more than that
	* can contain publication date or version history (how?)
	* <header> can include <nav>
	* <header> should include at least one of h1-h6,hgroup

* hgroup
	* represents the heading of a section
	* used to group h1-h6 tags when a heading has subheadings, taglines or alternative titles
	* it can **only** contain h1-h6 elements
	* when the document outline encounters a hgroup it will mask all but the highest level heading in the group

* nav
	* represents navigation in a document
	* it is a sectioning element
	* not all groups of links in a page need to be in a <nav> - only groups of **major** navigation links
	* not totally clear when exactly to use it but probably good for
		* table of contents
		* previous/next buttons
		* pagination
		* search form
		* breadcrumbs
	* <nav> is different from <menu>. <menu> is to be used for a list of commands and is an interactive element

* section
	* represents a generic document or application section
	* represents a thematic grouping of content
	* can contain a <header> and <footer>
	* examples: chapters in a book, tabs in a tabbed app interface
	* it is not a generic container element - you should use <div> for that
	* if you need a container for styling or scripting purposes you should use div
	* generally <section> should not be used if there is no natural heading for it (outliner tool can test this)
	* html5 doesn't have special tag to specify "main content" - it assumes that anything not marked as secondary content is main content
	* don't use <section> if <article>, <nav> or <aside> is more appropiate
	* it is a sectioning element so will affect the document outline

* article
	* <article> is a sectioning element (it affects the outline algorithm)
	* can contain a <header> and <footer> and other <section> tags
	* a section of a page that forms an independent part of the document (something that could be distributed independently and make sense)
	* smell test: does this piece of content make sense on it's own
	* an article can contain sections to divide it's content
	* an article is a special kind of section - it is an "independant" section
	* can contain <time pubdate> to indicate when it was published

* aside
	* represents a section of the document that contains content that is tangentially related to the content around it
	* when used within an <article>: the contents should be related to the article
	* when used outside an article:
		* the contents should be related to the site e.g.
			* blogroll
			* advertising
			* additional navigation
		* it can represent secondary content (often paired with role="secondary")
		* it represents content that is not the primary focus of the page (article) but is still related to the page (article)
		* it is often used for a sidebar but is not defined by it's placement in the document

* footer
	* represents the footer of a document or document section
	* often paired with role="contentinfo"
	* typically contains metadata about the enclosing document e.g.
		* author
		* copyright
		* related documents
		* contact info (should be marked up with <address>)
	* <footer> is not a sectioning element - it does not affect the document outline
	* don't interpret its name as presentational - it is not necessarily at the bottom of the document or document section (but often will be)
	* every section can have a footer (but does not have to)

* address
	* Represents the contact information for its enclosing section. If it is a child of the body element, then it applies to the document as a whole. The address element is not appropriate for all postal and e-mail addresses; it should be reserved for providing such information about the contact people for the document. ’ve introduced hCard here to reinforce that you are not supposed to use the address element for arbitrary postal addresses, but rather for providing contact information for a whole document or section of a document.

* figure
	* The figure element represents some flow content, optionally with a caption, that is self-contained and is typically referenced as a single unit from the main flow of the document.

* The figure element can be used to annotate illustrations, diagrams, photos, code listings, etc., that are referenced in the main content of the document, but that could, without affecting the flow of the document, be moved away from that primary content — e.g., to the side of the page, to dedicated pages, or to an appendix.

* figcaption
	* The figcaption element represents a caption or legend for the rest of the contents of the figcaption element's parent figure element, if any. The <figcaption> element is optional and can appear before or after the content within the <figure>. Only one <figcaption> element may be nested within a <figure>, although the <figure> element itself may contain multiple other child elements (e.g., <img> or <code>).
	* You should choose between <aside> or <figure> by asking yourself if the content is essential to understanding the section:
	* If the content is simply related and not essential, use <aside>.
	* If the content is essential but its position in the flow of content isn’t important, use <figure>.

* time
	* Represents a precise date and/or time in the proleptic Gregorian calendar. The time element encodes modern dates and times in a machine-readable way, so that, for example, user agents could offer to add an event to the user's calendar.
