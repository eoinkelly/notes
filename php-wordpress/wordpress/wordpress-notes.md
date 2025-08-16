# Inbox

http://codex.wordpress.org/Theme_Unit_Test
http://betterwp.net/wordpress-plugins/bwp-minify/
http://wp.tutsplus.com/tutorials/10-quick-tips-optimizing-speeding-up-your-wordpress-site/
http://wpdevel.wordpress.com/ http://wp-snippets.com/
http://www.thenile.co.nz/books/Mark-Bittman/How-to-Cook-Everything-2000-Simple-Recipes-for-Great-Food/9780764578656/
http://wp.smashingmagazine.com/2012/03/22/useful-wordpress-tools-themes-plugins/
awesome list of stuff from smashing mag awesome article on how wp boots up
http://theme.fm/2011/09/wordpress-internals-how-wordpress-boots-up-2315/
http://net.tutsplus.com/articles/scaling-wordpress-for-hi-traffic/
http://www.wpbeginner.com/wp-themes/default-wordpress-generated-css-cheat-sheet-for-beginners/
WP js loading enhancements

let minify honour header and footer placement requests for scripts i.e. it will
include 2 script groups, one in header, other just before </body> change to
support CDN loading first and then loading local copy if that fails as per
boilerplate find good way of loading script in conditional HTML comments
conditional script loading in WP

best htaccess practices:
http://wordpress.stackexchange.com/questions/18968/best-collection-of-code-for-your-htaccess-file?newsletter=1&nlcode=23595%7c7ce8
best functions.php practices:
http://wordpress.stackexchange.com/questions/1567/best-collection-of-code-for-your-functions-php-file

wordpress forms

page lines uses bbpress for forums jigoshop for ecommerce - free exommerce wp
plugin http://jigoshop.com/ themes and extensions are paid jigoshop is also the
forked basis for woocommerce
http://wpcandy.com/reports/woothemes-forks-jigoshop-into-woocommerce

contact form 7

- no html5 default placeholder="" support but it does have it's own watermark
  fallback
- does not support <label> as it does not assign an id to form elements

gravity forms http://www.gravityforms.com/purchase-gravity-forms/

- looks pretty

* $200 (includes only 1 year of updates)

# SEO

all in one seo does the following Canonical URLs Fine tune Page Navigational
Links Built-in API so other plugins/themes can access and extend functionality
ONLY plugin to provide full SEO Integration for WP e-Commerce sites Nonce
Security Support for CMS-style WordPress installations Automatically optimizes
your titles for search engines Generates META tags automatically Avoids the
typical duplicate content found on Wordpress blogs For beginners, you don't even
have to look at the options, it works out-of-the-box. Just install. For advanced
users, you can fine-tune everything You can override any title and set any META
description and any META keywords you want. Backward-Compatibility with many
other plugins, like Auto Meta, Ultimate Tag Warrior and others.

Here is what an ideal SEO plugin should od customise page title,
meta-description, meta-keywords site-wide and individually for each page have a
site-wide list of keywords that get added to all pages allow the rich-snippet
image do sensible things about avoiding duplicate content - need to research
this other stuff?

# WP Custom theme

    what is the best wp way to have a custom page with data filled in from a custom post type that the client can still move around in the menu??
    Are there reusable CSS objects here? (used elsewhere on site or on another site)?
    If so, can I separate them into structure and skin to make them as reusable as possible

    merge my wp theme base with the initial work I did on aspen lodge

    example site of ragged edges
    	http://omis.me/2011/07/31/tripping-the-google-maps-fantastic/?utm_source=rss&utm_medium=rss&utm_campaign=tripping-the-google-maps-fantastic

    new patterns
    	jquery carousel
    		https://github.com/aino/galleria
    		ux ideas: http://uxmovement.com/navigation/big-usability-mistakes-designers-make-on-carousels/?utm_source=twitterfeed&utm_medium=twitter&utm_campaign=Feed%3A+uxmovement+%28UXMovement%29G
    	logo image
    		http://csswizardry.com/2011/08/more-logo-markup-tips/

    wordpress theme pattern
    	I need to understand every line as I'll be customising it heavily
    	It needs to have basic blog & pages functionality out of the box
    	based on html5 best practices
    	a pattern for creating custom widgets
    	easily turn widget areas on/off
    	translatable
    		how do I make .pot file for my theme?

    themes being built
    	now
    		jono qbt
    		aspen lodge
    		troutstalkers

    	soon
    		central motorways
    		new swing site
    		allib.co.nz
    		eoinkelly.info
    		aed website

    	share css, js, img as much as possible with all template sites
    	im confused about h5bp js layout

    	2011 give me heaps of funcitons out of the box but I don't udnerstand it as well
    	so do I base on 2011 or h5bp

# Setting up media

This is important content setting.

Need to choose: thumbnail size: default 150x150px, should probably be square
medium size: default 300 max x 300 max large size: default 1024 max x 1024 max

with scissors contd. we can also choose whether to store the original upon
upload + this is the most flexible if we need to regenerate thumbnails for
future redesigns - it takes up a lot more hosting space - need to make sure the
original is not linked to on the site as it's massive

# Media Management

## Default wordpress behaviour

when you upload a file stores the original file with the name you uploaded it
creates a 150x150 thumbnail (hard cropped) creates a medium size that does not
exceed the dims you give it creates a large size that does not exceed the dims
you give it

the result is for images bigger thatn 300px in one dimension this image will
alwasy fit into a 300x300 px box. it will only fill the box if it is a square
image if the image is landscape it will be 300px wide if the image is portrait
it will be 300px tall for images smaller than 300px in either dimension

0 disables resizing in that direction force width to 0 if h < w (landscape)
force height to 0 if w < h (portrait)

for medium image, we ask for width = 300, height = 300

if the image is landscape we disable resizing the width if the image is portrait
we disable resizing the height

# 3 Ways to query the database

query_posts() alwasy finish wiht wp_reset_query() it does alter all post related
global variables & template tags

new instance WP_Query

get_posts() uses WP_Query to fetch posts

# Images required

images I need from designers for my wp starter theme 1000x288 header 230x66
header-thumbnail ??? wp-login page 300x225 wp custom theme thumbnail create this
myself based on my logo and their name

# page structure

[START page.php] [START header.php] body body_class() div #page .hfeed
.container header #branding .block role=banner hgroup .column .span-24 h1 h2 nav
#access .block role=navigation h3.assistive-text div.skip-link div.skip-link
div.menu ul li.current_page_item li.page_item page-item-22 (22 is the id of the
page) div #main block [END header.php] div#primary .column .span-18 div#content,
role=main [START content-page.php] article#post header.entry-header
h1.entry-title div.wysiwyg the_content() footer.entry-meta [END
content-page.php] [START sidebar-page.php]
div#secondary.widget-area,role=complementary aside#archives.widget
h3.widget-title ul aside#meta.widget h3.widget-title ul [END sidebar-page.php]
[START footer.php] footer#colophon,role=contentinfo get_sidebar(footer)
div#site-generator [END footer.php] [END page.php]

# Setup Steps

1. create a new db for the site
1. always use a hard to guess table prefix and good username and password
1. ? salting/ encryption stuff here?
1. Copy wp files to the appropiate place on server
1. remove readme.html & licence.txt
1.  3. remove config-sample.php
1. untick ‘allow site to appear in google/technorati’
1. choose a unique admin name
1. set up .htaccess
1. ? lots to learn here
1. install standard plugins
    1. wp-db-backup
    2. scissors continued
    3. google xml sitemaps
    4. google analyticator (or google analytics for wordpress
    5. wp-minify
    6. wp-super-cache
    7. search regex
    8. broken link checker
    9. redirection
    10. enable media replace
    11. all in one seo pack
    12. wordpress importer
1. security
    1. secure wordpress
    2. wp security scan
    3. wordpress file monitor
    4. ultimate security check
1. security-checks
    1. file pemissions
    1. check how media uploading works with that particular hosting so can tell
       client reliably what will happen
    1. ? need research here

use same name for local and remote db if poss - renaming is a pain don't change
table prefix once you have decided on it - it gets embedded in wordpress's
tables and is a pain to change an image in WP is a type of custom post
(type=attachement) - it has metadata etc. like a normal one

Final checks

1. allow site to feature in google/technorati
2. have a backup of files and db
3. need to have permissions such that client can update/add plugins/add media as
   appropiate - what are the consquences here?

# wordpress contants and paths

ABSPATH = the root of wordpress THEMEPATH get_template_directory_uri()

1. WP_CONTENT_DIR – eg: www/www/something/publichtml/wp-content
2. WP_CONTENT_URL – eg: http:domainname/wp-content
3. WP_LANG_DIR
4. WP_PLUGIN_DIR
5. WP_PLUGIN_URL
6. STYLESHEETPATH (in case a theme uses another as it’s template, this and the
   next can be different)
7. TEMPLATEPATH

There are some ‘constants’ that can be defined in wp-config, but are not then
defined if not set in wp-config. these are

1. WP_SITEURL (where the wordpress code is – eg: http:domainname/wp)
2. WP_HOME (what your website calls home – eg: http:domainname )

3. plugins_url: The most useful function to get the browser friendly URL of the
   plugin files.
4. plugin_dir_url: Get the URL of the plugin directory with a trailing slash.
5. plugin_dir_path: Server path of the plugin directory with a trailing slash.

get_theme_root_uri() Retrieve URI for themes directory. Does not have trailing
slash.

content_url()

# plugin_basename()

$allib_debug_constants = array( WP_CONTENT_DIR, WP_CONTENT_URL, WP_LANG_DIR,
WP_PLUGIN_DIR, WP_PLUGIN_URL, STYLESHEETPATH, TEMPLATEPATH, WP_SITEURL, WP_HOME,
ABSPATH, THEMEPATH );

var_dump($allib_debug_constants);

array 0 => string 'C:\Users\Oi\Documents\My
Dropbox\websites\aspenlodge.co.nz/wp-content' (length=69) 1 => string
'http://localhost/aspenlodge.co.nz/wp-content' (length=44) 2 => string
'C:\Users\Oi\Documents\My
Dropbox\websites\aspenlodge.co.nz/wp-includes/languages' (length=80) 3 => string
'C:\Users\Oi\Documents\My Dropbox\websites\aspenlodge.co.nz/wp-content/plugins'
(length=77) 4 => string 'http://localhost/aspenlodge.co.nz/wp-content/plugins'
(length=52) 5 => string 'C:\Users\Oi\Documents\My
Dropbox\websites\aspenlodge.co.nz/wp-content/themes/aspen-lodge' (length=88) 6
=> string 'C:\Users\Oi\Documents\My
Dropbox\websites\aspenlodge.co.nz/wp-content/themes/aspen-lodge' (length=88) 7
=> string 'WP_SITEURL' (length=10) 8 => string 'WP_HOME' (length=7) 9 => string
'C:\Users\Oi\Documents\My Dropbox\websites\aspenlodge.co.nz/' (length=59) 10 =>
string 'THEMEPATH' (length=9)

1. plugins_url( ) — Full plugins directory URL (for example,
   http://example.com/wp - content/plugins )
2. includes_url() — Full includes directory URL (for example,
   http://example.com/wp - includes )
3. content_url( ) — Full content directory URL (for example,
   http://example.com/wp - content )
4. admin_url( ) — Full admin URL (for example, http://example.com/wp - admin/ )
5. site_url( ) — Site URL for the current site (for example, http://example.com
   )
6. this is the path to your wordpress core files - it is not necessairly the
   same as home_url()
7. home_url( ) — Home URL for the current site (for example, http://example.com
   )
8. this is the url you want the public to use to get at your site.

The site_url( ) and home_url() functions are similar and can lead to confusion
in how they work. The site_url() function retrieves the value as set in the
wp_options table value for siteurl in your database. This is the URL to the
WordPress core ﬁles. If your core ﬁles exist in a subdirectory /wordpress on
your web server, the value would be http://example.com/wordpress .

The home_url( ) function retrieves the value for home in the wp_options table.
This is the address you want people to visit to view your WordPress web site. If
your WordPress core ﬁles exist in /wordpress , but you want your web site URL to
be http://example. com the home value should be http://example.com .

The plugins_url( ) function will be one of your best friends when building
plugins in WordPress. This function can help you easily determine the full URL
to any ﬁle within your plugin directory. < ?php plugins_url( $path, $plugin ) ;
? >
