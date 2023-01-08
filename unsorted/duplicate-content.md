Duplicate Content
=================

*   http://yoast.com/articles/duplicate-content/
*   http://www.lunametrics.com/blog/2009/02/02/hashing-it-out-referral-tracking/

link:www.google.co.nz
site:foo.com intitle:"keyphrase x"

disable comment pagination on wordpress to avoid dupe content?


SEO duplicate content checklist
    choose whether you want www or bare url - get client to promote & use one consistenly
    make sure apache issues 301 redirect from the wrong one to the right

Choosing between www and non-www
================================
    how do these relate to each other?
        in .htaccess get it to re-write any urls
        tell webmaster tools which one is your choice via webmaster tools interface
        use canonical meta tag

http://support.google.com/webmasters/bin/answer.py?hl=en&answer=139066
http://support.google.com/webmasters/bin/answer.py?hl=en&answer=139394

if a <meta> tag has a hyperlink as it's value then the more correct name for it is <link>
<link rel="canonical" href="http://www.example.com/product.php?item=swedish-fish"/>
can also use a HTTP header for non HTML content e.g. pdfs
recommend using absolute links in it
it is just a suggestion to google
google can follow a chain of canonical links to "some extent"