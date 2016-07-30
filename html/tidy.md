
An attempt to craft a set of options to use html-tidy on ERB templates

/usr/local/bin/tidy -indent -bare -quiet --show-body-only yes --tidy-mark no --wrap 0 --output-xhtml true --sort-attributes alpha -m file.html.erb


Conclusion: the above doesn't really get it right
