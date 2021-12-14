# Cloudinary

* can upload any file type just for storage (even if it's not a media file)
* note that asset changes are **on the fly**, not just at ingest
* accounts can have sub-accounts
* account identifiers
    * cloud name
        * can be cahnged by contacting support team
        * is public, included in all URLs of resources
    * support may ask for last 4 chars of API secret
* they have a wordpress plugin for letting wordpress pull assets from
* the folders you put stuff in appear in the public URL of content
* you can import from facebook and insta and shutterstock
* if you have backups configured you can get at deleted files for 30 days
* cloudinary has magic urls segements that deliver the image athe optimal quality or file format for a device
    * https://res.cloudinary.com/ackama/image/upload/f_auto,q_auto/v1576714909/bert-head_deacii.png
    * its the f_auto, q_auto (format and quality respectively) segments in the URL
* you can set an notificaiton URL when an upload preset fires
* you can set some transformations to happen eagerly at upload via an upload preset (rather than lazily later)
* Pro tip: use named transformations to make your urls more stable and hide the specifics of your transformations
* the support the following file types
  1. Images
  2. Videos
  3. Raw files (basically anything else you upload)
* Don't add filetype to the public-id because you can make many filetypes from it so you don't want to bake a filename suffix
* the folder name is effectively part of hte _public id_ even though you can specify them separately in the APIs
* you can upload from base64 URI, S3 bucket, FTP, local filesystem, HTTP or HTTPS
* you can map a cloudinary folder to a remote URL so when you access an imagein the cloudinary folder it will be retrieved from the report URL and stored in the folder
    * useful for migrating from an existing source of assets
* images are made available over http and https
* they have active_storage integration and support direct uploads
  * https://cloudinary.com/documentation/rails_activestorage
* a lot of the API is sugar for creating the correct URL to get the version of the image you need
* when you configure S3 bucket as backup, they manage the backups but use your bucket
* you can specify that authentication is required to fetch the image
* you can upload .ia, .psd, .indd adobe files as supported formats
* you can add the version of the asset in the URL to bypass CDN caching of the "latest" version
* don't create folders named in form `v\d+` because it looks too much like the version string
* incoming transformations
  * eager = apply on upload but keep original file
  * incoming = apply on upload and discard original file - only store the transformed file
QUESTIONS

    what content moderation do they have?
    can you export to social media apps easily?
    should we demo the "image analysis" tab by being a premium customer to RNZ?
    which cloudnary plan are we suggesting for them?
    what cms'es do they use to present info to customers?
    can we upload photoshop files and have them generate jpgs etc.???
    can it automatically pull info from exif data?

DEMO IDEAS:

* emphasise how many sdks cloudinary has => future dev has lower cost
* show using the upload widget on an existing site
* a key selling piont is how much is already done by cloudinary
* show parsing metadata out of a jpg and creating tags from them
* show using url transformations to do transformations on the fly - maybe a non-designer working on a powerpoint and trying out some ideas
* show uploading from a mobile device too
* show a workflow from lightroom to cloudinary
* setup an S3 inbox - maybe that would let us automate from
* show generating a webp as well as a jpg and show that being pulled in by the browser

how to add an existing cloudinary user to my cloud


https://res.cloudinary.com/ackama/image/upload/v1576714909/Plan9%20Bunny.jpg

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

## Search API

* search expressions are lucene like
    * https://cloudinary.com/documentation/search_api#expressions
* sort
    * default is creation_date desc
* max_results, max is 500
