# defer and async

```
<script defer src="foo.js"></script>
<script async src="foo.js"></script>
```

* default script tag
    * browser has to
        1. stop parsing HTML
        2. download the JS file
        3. parse the JS file
        4. execute the JS file
        before it can parse the next line of HTML

* async
    * browser support is very good (except IE9 and older)
        * http://caniuse.com/#feat=script-async
    * "hey browser, download this script in parallel and run it as soon as you can"
    * continue parsing the HTML but execute me ASAP
    * has garuanteed access to the document tree above its script tag
    * no IE6-9 support, otherwise good support
    * no garuantees about what order the scripts are executed in.
    * only valid for external JS files

* defer
    * browser support is very good (except IE9 and older)
        * http://caniuse.com/#feat=script-defer
    * says to browser "I want you to execute this after the HTML document has been fully **parsed**".
    * This allows the browser to download the JS file in parallel with continuing to parse the DOM
    * "defer running this script until the parsing has finished"
    * script has access to full document tree
    * run *before* DOMContentLoaded event is triggered
    * no IE6-9 support, otherwise good support
    * scripts marked with defer are executed in the order they appear in source
    * must **NOT** use document.write() as the browser will blow away the document
    * ignored on inline scripts by at least FF
    * You cannot **depend** on the script being deferred but you can ask.
    * only valid for external JS files
