
UPloading

1. Public
	* original file is public
	* transformed versions are public
2. Private
	* original file is accessible only with signed URL
	* transformed versions are public
		* => anybody can ask for any transformation they can think of
		* turn on "strict transformations" to restrict transformations
	* Cloudinary can generate a time-limited signed URL to the original asset with the `private_download_url` method
		* this works for "private" assets but not "authenticated" I think
3. Authenticated
	* more private than "private"
	* original file requires "authentication"
	* transformed versions requires "authentication"
	* "authentication" is one of
		1. A signed URL
		2. Token based auth (can restrict access based on time, IP address or ACL based on the path in the public ID)
		3. Cookie based auth (similar feature set to token based auth)
	* asset uploaded with `type: authenticated` will have that baked into the URL - it is always authenticated
	* you can temporarily set an asset to "authenticated" via the `access_mode` and `access_control` params to Upload and Update methods


When strict transformations is enabled you need to do one of:

1. eagerly generate all derived assets
2. mark specific transformations as allowed
3. use signed URLs

to access the asset.

What are the consequences of uploading audio as "authenticated"?

* we must always use a signed URL to access it
* => anything which generates URLs needs to have access to the Cloudinary API key