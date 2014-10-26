# Facebook API

Facebook dev tools: https://developers.facebook.com/tools/


Uses a _variant_ of Oauth 2.0

Permissions

You ask for permissions during the _login_ process OR you can ask for them later.
The only permissions you can ask for without review are

1. public_profile
2. email
3. user_friends

* All other permissions require Facebook to review and approve the app before
  the request for those permissions will become visible in the login dialog.
* Developers are excempt from this rule
    * Consequences: Test app on more than just developer accounts!!!
* To test what permissions your app has for a given user issue `GET /{user-id}/permissions`
* Users may deny the permissions you ask for so we have to check that we actually
got what we asked for.

an app can revoke permissions that a user has given it:

    DELETE /{user-id}/permissions/{permission-name}

or it can revoke access entirely:

    DELETE /{user-id}/permissions

Both the above Graphs API requests must be made with a valid _user access
token_ or a valid _app access token_.


## Anatomy of an access token

There are 2 types of access token

* User = used by humans
* App = used by apps

An access token

* is a _variable length_ char string that FB associates with
    * App ID
    * User ID
    * Issued
    * Expires
    * Valid
    * Origin
    * Scopes
        * the permissions this user has _granted_ to this app (the app may have _asked_ for more)
        * e.g. public_profile, email
* it usually has a lifetime of a few hours but you can get a "long lived token" if you are accessing facebook.com from a server.

    TODO: find out about this


## App settings

### Basic Settings Tab

* App ID
    * Unique ID code for the appp
    * Used to initialize the SDKs
* App Secret
    * secret "password" that the app has embedded in its code so that
      facebook.com can identify requests from it
    * If using a native facebook SDK these are stored in the plist/properties
      file in the project
* Display Name
    * Pretty name for use in UI presented to the user
* Namespace
    * the namespace uniquely identifies _objects_ and _actions_ of your app on Facebook.
    * The namespace you choose will be used to identify objects specific to
      your application (e.g. yourappns:meal) and actions published to a
      Timeline (e.g. yourappns:eat).
    * is also the short app name used for your Canvas Page URL (e.g., apps.facebook.com/YOUR_APP_NAMESPACE)
* App Domains
    * can associate a DNS domain with the app.
    * possibly only needed for apps hosted elsewhere but displayed on facebook.com ???
    * `TODO: for what reason ???`
* Contact email
    * The email address that is linked to by facebook.com as support for the app.
    * used by facebook for all app related communications

### Advanced Tab

* Native or Desktop app boolean
* Deauthorize Callback URL
    * A URL that facebook will "ping" when a user deauthorizes the app
* Age restrictions
    * Contains Alcohol
    * Age Restriction
    * Social Discovery
        ???
    * Country restricted
        ???
* Security
    * Servier IP Whitelist
        * a ? separated list of IP addresses that app requests using the app secret must come from
    * Update settings IP whitelist
        * a whitelist of IP addresses that can lock down where "app settings" are updated from
        ??? are app settings only editable from developers.facebook.com
    * Update notification email
        * Who to email when app settings change
    * Auth settings
        * Client token
        * enable client oauth login
        * boolean: app secret proof for server API calls
        * Embedded browser Oauth login
        * Require 2-factor reauthorization for changing "application settings"
        * Valid Oauth redirect URIs
            ????
* App-scoped User ID sharing
* Advertising Acocunts
* Insights
    * install insights
    * Enable enhanced interest targetting
* Migrations
    * A set of boolean flags that controls which of the currently in progress API migrations are turned on for your app.
    * Varies depending on what migrations Facebook is currently doing


## Platforms

An app can have many _platforms_. In 2014 platforms are one of

1. Website
2. Android App
3. iOS App
4. Facebook Canvas
5. Windows App
6. Playstation
7. XBOX
8. Playstation


### Website section

The config here only applies to the JS SDK

* Site URL
    * Guess: used by facebook for knowing where to redirect the browser to after auth
* Mobile Site URL (optional)
    * Assumption: If you have a separate mobile domain, then facebook can send
      people back to it instead if they came from the mobile login flow.


### iOS Section

* Bundle ID
* iPhone Store ID
* iPad Store ID
* URL Scheme suffix (optional
* Single sign on boolean
* Deep linking boolean

### Android section

* Package Name
* Class Name
* Key Hashes
* Single sign on boolean
* Deep linking boolean


# Test users

* are invisible to real accounts
* used for automated testing of an app
* 2 ways to create:
    1. via graph API
    2. manually via dashboard
* test users can only interact with other test users
* they cannot become a fan of a page
* can only be accessed by developers of the associated app
* each app can have max 2000
* cannot be converted to normal accounts
* Will automatically have the "tester" role (and associated privileges)

# Test apps

* all test apps share the same app-scoped user ID namespace as the production app
    * makes it simpler to debug issues with app scoped Ids
* are always in development mode
* have same _platform migration_ options as the production app they were copied from
    * allows you to test migrations
* Have the same version availability as a prodction app
* test apps are _children_ of a production app

# App review

* forces mobile apps to use the native SDK
* review time is approx 3-7 business days
* we need to provide test users for the reviews to use


# API versioning

* Not all APIs and SDKs share the same version e.g. graph API is versioned at a
  different pace and numbering compared to the iOS SDK.
* All SDKs can interact with multiple versions of APIs
    * Core - will stay stable for 2 years
        * e.g. SDKs, Login dialog, Share dialog, Requests dialog, _some_ Graph API endpoints
    * Extended
        * e.g. Ads Pages
        * are subject to _Migrations_
        * a migration is the period of time where both the old and new behaviour is available
        * you can turn them on/off via the graph API or via the App dashboard (during the 90 day period)
        * may migrate every 90 days

* If you don't specify an API version in your call (aka an "unversioned" call)
  you will get the oldest available version of the API
    * implications: always specify a version
    * SDKs automatically use whatever version is most recent when they were downloaded

## The SDKs

* Javascript:
    * `FB.init` you pass the version you want as a parameter
* iOS:
    * the version of the API you use is hard-coded to be the most recent
      available when that version of the SDK was released
    * you can override with `[FBRequest overrideVersionPartWith]`
* Android
    * the version of the API you use is hard-coded to be the most recent
      available when that version of the SDK was released
    * can override with `Request.serVersion()`

In all the SDKs the version is prepended to the API calls e.g. `me/events`
becomes `/2.0/me/events`


# Manual login (not using an SDK)

https://developers.facebook.com/docs/facebook-login/manually-build-a-login-flow/v2.1
