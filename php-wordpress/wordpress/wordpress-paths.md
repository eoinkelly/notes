

The Preferred Medhod
====================
get_template_directory_uri() will return the parent theme dir url
get_stylesheet_directory_uri() will return the child theme dir url
get_template_directory() will return the parent theme dir path
get_stylesheet_directory() will return the child theme dir path

older methods
=================
<?php bloginfo('template_directory'); ?>/js/mycooljs.js
	no longer prefered - use get_template_directory_uri/get_stylesheet_directory_uri instead




the constnats
===================
The default wordpress themes use the functions in case somebody wants to filter that path - setups like WP.com do that - this means that the contstants are not recommended for themes "in the wild" - use the functions - see http://core.trac.wordpress.org/ticket/18071 - the filter is called 'template_directory_uri' filter

Wordpress maps the functions above to some constants for us. The advantages of this are less PHP function calls and potentially less DB queries. I don't know how much diff this makes if you are gonig to cache anyway. You can potentially increase performance more by hard-coding them

wp-includes/default-constants.php:289:  define('TEMPLATEPATH', get_template_directory());
wp-includes/default-constants.php:295:  define('STYLESHEETPATH', get_stylesheet_directory());

As is, these two definitions are still querying the database, but we can eliminate these extraneous queries by hardcoding the values into place:

Could add to wp-config.php:

define('TEMPLATEPATH', '/absolute/path/to/wp-content/themes/active-theme');
define('STYLESHEETPATH', '/absolute/path/to/wp-content/themes/active-theme');