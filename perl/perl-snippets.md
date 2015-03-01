# Perl Snippets

Remove non-ascii from a file

	perl -i.bk -pe 's/[^[:ascii:]]//g;' <filename>

Search & Replace across all PHP files in current dir

	perl -pi -w -e 's/wrong/right/g;' *.php