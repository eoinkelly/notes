# Moving A WordPress Installation

- Make sure that WP_SITE_URL and WP_HOME are set to the new site in
  wp-config.php
- Run search & replace on the posts db and replace all instances of old url with
  new
- Change .htaccess to adapt to new dir (if required)
