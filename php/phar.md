# Phar files

* JAR archives for PHP basically
* lets you put a whole PHP app in a single file
* you can reference files within phar files from a php script

All Phar archives contain three to four sections:
    a stub
    a manifest describing the contents
    the file contents
    [optional] a signature for verifying Phar integrity (phar file format only)
* Phar files can use tar or zip

