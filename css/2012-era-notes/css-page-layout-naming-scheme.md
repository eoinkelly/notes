
TODO
    sort out a consistent naming scheme for common page structures

{page-area}-content contains the actual content


iedally the class names should refer to the type of "content" they contain, not their place on the page as that may change on diff devices

Page Layout 1: Separate Header, Main-content, Footer
----------------------------------------------------

html
    body.{the usual WP classes}
        div#site-header-container.site-header-container
            header#site-header.site-header
        div.main
            div#primary.primary
                div#content.content,role=main
                    {custom stuff for each site}
            div#secondary-content
                {custom stuff for each site}
            div.tertiary-content
                {custom stuff for each site}
        div#site-footer-container.site-footer-container
            footer#site-footer.site-footer
                {custom stuff for each site}


Page Layout 2: Header, Main-content, Footer all wrapped in a page container
----------------------------------------------------

html
    body
        div.page-container
            div.header-container
                div.header
            div.main
                div.main-content
            div.footer (w: 100%, h:content-based)
                div.footer-content


Page Layout 3:  Header, Main-content wrapped in page-contaier, separate Footer (possibly sticky)
----------------------------------------------------

html
    body
        div.page-container (w: 100% of viewport, h:content-based)
            div.header (w: 100%, max-w:960px, h:content-based)
                div.header-content
            div.main (w: 100%, h:content-based)
                div.main-content
        div.footer (w: 100%, h:content-based)
            div.footer-content