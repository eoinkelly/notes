Inbox
=====
http://sucuri.net/how-to-stop-the-hacker-by-hardening-wordpress.html
http://wp.smashingmagazine.com/2010/07/01/10-useful-wordpress-security-tweaks/
good article on using htaccess to secure http://journalxtra.com/websiteadvice/wordpress-security-hardening-htaccess-rules-4025/
the ultimate security checker plugin has some good tips for fixing problems
plenty of info in http://codex.wordpress.org/Hardening_WordPress
Never ever send passwords over email - think of email like sending a postcard
http://codex.wordpress.org/Changing_File_Permissions#Permission_Scheme_for_WordPress - good article on wp file permission scheme

Questions
=========

questions for hosting:
	can I chown via web/ftp interface?
	is each ftp user I create a real separate user on the system
	what user does the web server & php run as.
	do they have any plans to enable SFTP
	how many accounts per server?
	how much would it cost to turn on SSL admin for all our hosted sites?


htaccess to hide wp-admin from all but certain ips http://www.mattcutts.com/blog/three-tips-to-protect-your-wordpress-installation/
test out the websitedefender scan thingy
get wp to not use it's version as a cache buster for versioning css and js files?


investigate moving wp-content?
do my plugins work with a moved WP_CONTENT?
how much extra security does a moved WP_CONTENT give you?

Ongoing Maintenance Tasks
=========================

*	Update plugins and core
		Do we do it or them? prob them on low plans, us on higher plans

		steps:
			take a db backup
			take a file backup
			update plugins and core
		30 mins minimum

*	Get database backups emailed every X
		can these be verified?
			gunzip, check
		can these be scanned for spamword, malware?
		need 30 mins to investigate what's going on if something unexpected
		database backup emails shold be sent to the client (and us on higher plans)

*	Get file change summary email every X
		should go to whoever is doing the updates
		need 30 mins to investigate what's going on if something unexpected

Initial Security Setup
======================
*	Setup file monitor to run daily and email ??? on change.
		us on big plans, client on small plan
*	Install and leave running X security plugins
		websitedefender wordpress secut
		Wordpress Firewall 2
		Wordpress File Monitor Plus
		Login Lockdown
* 	Install, run, delete
		Ultimate Security Checker
*	setup google webmaster tools to forward emails to whoever is taking care of security


Monitoring
==========
*	if all my pages validate html then not validating *might* indicate a problem

4 Basic Pillars of WP Security
1.	Backups (offsite)
2.	Passphrases (good ones)
3.	Updates
4.	Monitoring

High-end monitoring and backup services
---------------------------------------
*	vaultpress (backup and monitoring and 0-day fixes pushes)
*	backupbuddy (backup only)
*	http://sucuri.net/signup (monitoring only, also provide a cleaning service)
*	websitedefender (cheaper, monitoring only)

Cleaning a hack
===================
can check the site here: http://sitecheck.sucuri.net/scanner/


You must take the code offline to clean it or it might re-infect as it goes
Change all passwords!
Change salt keys
Tip: chmod 0 makes a file unreadable without deleteing it!

http://smackdown.blogsblogsblogs.com/2008/06/24/how-to-completely-clean-your-hacked-wordpress-installation/
http://www.netcorps.org/kb/wordpress/disinfecting-wordpress

Cleaning the database
---------------------

http://blog.sucuri.net/2011/02/cleaning-up-an-infected-web-site-part-i-wordpress-and-the-pharma-hack.html

We are looking for
*	executable code (php or javascript)
*	unwanted content (html, css), possibly hidden
*	embedded iframes

connect to db
foreach table in db
	foreach column
		if it can contain text
			if it's plain text
				dump it
			if it's json
				decode it and dump it

VARCHAR
TEXT
BLOB

wp_postmeta
wp_post
wp_options
which wordpress columns store plain text?

SELECT post_content,post_title FROM `dirty`.`wp_posts`;
select option_name,option_value from wp_options;

-- get rid of transients
DELETE FROM `wp_options` WHERE `option_name` LIKE ('_site_transient%')
DELETE FROM `wp_options` WHERE `option_name` LIKE ('_transient%')

	SELECT * FROM wp_posts WHERE post_content LIKE '%<iframe%'
	UNION
	SELECT * FROM wp_posts WHERE post_content LIKE '%<noscript%'
	UNION
	SELECT * FROM wp_posts WHERE post_content LIKE '%display:%'
	UNION
	SELECT * FROM wp_posts WHERE post_content LIKE '%base64_decode%'
	UNION
	SELECT * FROM wp_posts WHERE post_content LIKE '%eval%'
	UNION
	SELECT * FROM wp_posts WHERE post_content LIKE '%gzinflate%'
	UNION
	SELECT * FROM wp_posts WHERE post_content LIKE '%<script%'
	UNION
	SELECT * FROM wp_posts WHERE post_content LIKE '%FilesMan%'

fingerprinting hack code
------------------------

Local file injection
Remote file injection
SQL injection

	<iframe
	<noscript


PHP:
	base64_decode()
	eval()
	gzinflate()
Specific to known hacks:
	FilesMan


for each table
    select everything
        decode the json if it is json


Trusted Hosting
===============
*	Do they patch their OS, Apache, PHP, PHPMyAdmin, mysql etc.
*	How much access do other users on the same host have? (this is prob why they don't offer SSH access)
*	Ideally we would want SFTP/SSH

Web Server User
===============
What user does the web server run as, what access does it have to our users files? waht perms can I get away with
does a new ftp user mean a new system user?
how does PHP run? is it phpsu? or a separate process to the server?


Database
=========
*	use separate database with sep user/pass for each wp installations to contain any possible breach
*	otherwise once the database is breached, all the sites in it are breached

On hd.net.nz it gets
	Select
	Insert
	Update
	Delete
	Create
	Drop
	Alter
	Index
	Reference
	Lock Tables
it does not get
	Grant


Installation
============
	remove the files for any unused plugins to decrease the attack surface area
	do not use 'admin' as the administrative account name - there should be NO admin account
		set to display firendly name on the blog not the username
	hide wordpress version info from the front end-how much does this really work?
	disable atom, rpc if i'm not using them
	disable user registraiton if I don't need it.

	codex.wordpress.org/Spam_Words = a list of words you can tell you wp install to flag as problems in spam

wp-config.php
=============
	always use a unkque table prefix
	set DISALLOW_FILE_EDIT in wp-config.php
	always set the wp salt in wp-config
	put wp-config.php outside web area if possible (i.e. wordpress is in web root)
	make sure all db and debugging error turned off in production code
	can move WP_CONTENT_URL/WP_CONTENT_DIR but some plugins/themes don't support it

File Permissions
================
for perms on the uploads folder
	ideally 755
	then try 775
	then try 777
it depends on how the hosting is setup

htaccess
========
set
	Options -ExecCGI
on the uploads and cache dirs

Plugins
=======
	*	wp exploit scanner
		by donnagh o caoimhe
		gives way too many false positives
		maybe possible to filter out the know false positives?

	*	secure wordpress by websitedefender
		might be a subset of BWP (next entry)

	*	WP Security Scan by websitedefender
		looks good

	*	WebsiteDefender WordPress Security 0.6 by websitedefender
		seems to combine wp security scan, secure wordpress features

	*	better wordpress security
		http://wordpress.org/extend/plugins/better-wp-security/
		makes some wp-config.php edits

	*	wp file monitor plus
		no good once an attack has occured
		might be useful as a way to be alerted if something goes wrong

	*	Ultimate Security Checker
		loosk pretty good
		scans core files pretty well

	*	wp security scanner
		http://wordpress.org/extend/plugins/wordpress-firewall-2/screenshots/

	* 	Login lockdown
	*	Bad Behaviour
	Wordpress HTTPS
	Limit Login Attempts


exteme measures
	htaccess password on the admin folder - it does break some admin functionality
		http://www.nicolaskuttler.com/post/htaccess-protect-wordpress-admin/
	get hosting with ssl access and hten force ssl on all login/admin pages

coding
=======

use the right esc_  function for the right context
name scheme is esc_{context}()
can also
esc_attr()
esc_url()
esc_url_raw() = doesn't encode ampersands, use when you are not outputting to html but saving to the db etc.
esc_html()
	use wp_filter_kses() instead if you want people to use some safe html, expensive function, run it on safe not load
esc_js() = used when using PHP to echo values into javascript
esc_textarea() = double encodes entites
esc_sql()
	sorts out SQL injection
	doesn't work for integers as there is not escape sequences for them in SQL
	$wpdb->prepare() is really good alternative


$url = "javascript:evil()";
This is WRONG: <a href="<?php esc_attr($url); ?>">blah</a>
This is CORRECT: <a href="<?php esc_url($url); ?>">blah</a>

make sure that your user has the minimum permissions
current_user_can('capability');
attach yourself to an existing capability is a good idea

CSRF ("sea-surf") Cross site request forgery
attacker uses your logged in session to do an aciton you are authorised to do but did not intend to do.
wp_nonce_field('pluginslug-actiontoperform_' . $object) combats this
check_admin_referer('pluginslug-actiontoperform_' . $object)

you can use the wordpress kses to whitelist html tags wihtout giving hte user unfiltered_html capability
kses is an expensive function - run it on save, not on output!

$wpdb->{functions} are the best safe way to deal with the db
use $wpdb->prepare() i fyou need a query that can't be done with the ->insert() etc. functions

principles of good dev
	1.	escape late
		There is not such thing as a "general escape" - there is only escaping in a particular context.
	2.	Anything that isn't hardcoded is suspect/Mistrust everything as hardcoded can change over versions
		This means you should escape EVERTYGHING

		esc_{context you are escaping for}_{optional suffix-can do translation and escaping in one step}()

		esc_html()

Never ever use eval()!

test form fields by searching for VALUE="" as these are easily exploited

Never just echo server global variables
<?php echo esc_attr($_SERVER['REQUEST_URI']); ?>