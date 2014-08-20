
## Examples

    <script defer src="foo.js"></script>
    <script async src="foo.js"></script>

default script tag =
  * browser has to stop and download it and executed it before it can parse the next line of the HTML

async =
  * continue parsing but execute me asapo
  * has access to the document tree above its script tag
  * no IE6-9 support, otherwise good support
  * no garuantees about what order the scripts are executed in.
  * "hey browser, download this script in parallel and run it as soon as you can"
  * only valid for external JS files

defer =
  * says to browser "I want you to exectue this after the document has been **parsed**". This allows the browser to download it in parallel
  * "defer running this script until the parsing has finished"
  * has access to full document tree
  * run *before* DOMContentLoaded event is triggered
  * no IE6-9 support, otherwise good support
  * scripts marked with defer are executed in the order they appear in source
  * must **NOT** use document.write() as the browser will blow away the document
  * ignored on inline scripts by at least FF
  * You cannot **depend** on the script being deferred but you can ask.
  * only valid for external JS files
