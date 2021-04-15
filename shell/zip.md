# Zip family of commands

* built-in to macOS
* there are a bunch of separate commands

Commands

* zip
    * create zip archives
* unzip
    * extract files from a zip archive
* zipinfo
    * show info about existing files (kind of like `ls -l` for the zip file)
* zipdetails
    * show debugging level info about the file
* zipcmp
    * compare zip archives
* ziptool
    * modify zip archives
* zipcloak
    * encrypt entries in a zip file
* zipnote
* zipgrep
* zipmerge
* zipsplit

Examples

```bash
# -r = find files recursive
# -X = exclude macOS special files like .DS_Store
$ zip -rXv output.zip *
$ zip -rXv output.zip ./things

$ zipinfo output.zip

# add -v for verbose
$ unzip -v output.zip
```

