# Permissions

https://cloudinary.com/documentation/image_upload_api_reference

## Delivery type ('type' in JSON)

* Has 3 possible values
  1. Public
    * original file is public and all transformed versions are public
    * this is the default
  2. Private
    * original file is accessible only with signed URL
    * transformed versions are public
      * => anybody can ask for any transformation they can think of unless you also turn on "strict transformations" to restrict transformations
    * You can temporarily flip a private asset to be public
      * Cloudinary can generate a time-limited signed URL to the original asset with the `private_download_url` method
        * this works only for "private" assets but not "authenticated" - use `access_mode` to flip authenticated assets
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

## access_mode

https://cloudinary.com/documentation/upload_images#private_assets

* values are `authenticated|public`
* The delivery type is baked into the URL
* access_mode lets you flip an asset between "public" and "authenticated" without changing the URL
  * Note you cannot flip to/from the "private" delivery type
* access_mode
  > The access_mode parameter allows a resource with the delivery type upload to
  > behave as if it's of type 'authenticated' while still using the default upload
  > delivery type in URLs. The asset can later be made public by changing its
  > access_mode via the Admin API, without having to update any delivery URLs. Valid
  > values for the access_mode parameter include public (default) and authenticated.

## access_control

* different to access_mode
* > The access control option is a premium feature currently available only for accounts with an Enterprise plan and is enabled upon request.
* must be enabled on your account
* if enabled you can configure it in the web GUI
  * this makes it unique - delivery type and access_mode can only be changed via API
* I **think** the only way to programmatically set access control is with an upload preset
* You can set `access_control` at upload https://cloudinary.com/documentation/image_upload_api_reference#:~:text=Default%3A%20upload.-,access_control,-JSON
* Can change asset between 2 states
  1. public
  1. restricted
    * can be permanently restricted
    * can set a start date and (optional) end date where access will be public
    > Setting an asset as Restricted means that people can only view that asset
    > outside the Media Library if they have both the asset URL and an
    > authentication token, except during an optional time-limited date range when
    > the asset is defined as publicly accessible.

?? does access_control change in derived fields vs original??
?? how does access_control combine with access_mode if you set them both? would that be a mad thing to do?