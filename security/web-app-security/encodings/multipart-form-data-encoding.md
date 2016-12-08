# multipart/form-data

* This encoding must be used when you are uploading a file via HTML form
* Uses mimetype `multipart/form-data`
* your HTML form must specify `enctype="multipart/form-data"` to choose this encoding
* most server-side libs transparently handle forms encoded with this or the default `application/x-www-form-urlencoded` encoding
* the encoded output looks a lot like Mime encoded attachments in email
    * not a concidence - once upon a time you could submit HTML forms via email
* `Content-Type` header of the request specifies the mimetype AND the string to use for boundary

    Content-Type: multipart/form-data; boundary=SOME_BOUNDARY_STRING
