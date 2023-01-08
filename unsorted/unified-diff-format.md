Reading Unified Diff Format
---------------------------

* the file starts with a header that identifies the path to each file
* original file path preceeded by --- and new file path preceeded by +++
* The file is divided into sections called "change hunks" -
* each hunk begins with "range information": a line enclosed by @@
*

@@ -l,s +l,s @@ optional section heading
l = starting line number
s = num oflines the change hunk applies to
@@ -[range info for original file] +[range info for new file] @@ optional section heading


* After that we get 3 lines of context on either side of the change
* changes are indicated by lines beginning with + and -
* + => an addition
* - => a subtraction

Example:

--- C:\Users\Oi\Documents\My Dropbox\ALLIB\Clients\ella-mac\chef-brendan\chefbrendancatering.com\htdocs\wp-content\themes\chef-brendan\style.css Wed Jul 25 02:24:36 2012
+++ c:\users\oi\appdata\local\temp\sublime-sftp-diff-1348082437\htdocs\wp-content\themes\chef-brendan\style.css Wed Sep 19 20:20:46 2012
@@ -215,7 +215,7 @@

 .mod-welcome { overflow: hidden; *zoom: 1; float: left; max-width: 33.3%; }
 @media screen and (max-width: 537px) { .mod-welcome { float: none; max-width: 100%; } }
-.mod-welcome .message { overflow: hidden; *zoom: 1; color: white; line-height: 1.3; padding: 2.5em 11% 0 11%; padding: 0 11% 0 11%; }
+.mod-welcome .message { overflow: hidden; *zoom: 1; color: white; line-height: 1.3; padding: 3.5em 11% 0 11%; padding: 0 11% 0 11%; }
 .mod-welcome .message > p { font-weight: bold; font-family: "Droid Serif", serif; font-size: 16px; }
 .mod-welcome .message > p:first-child { margin-bottom: 0; }
 @media screen and (max-width: 800px) { .mod-welcome .message { padding-top: 1.5em; } }