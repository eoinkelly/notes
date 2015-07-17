# API design

Github API is seen as a good one.

Docs: https://developer.github.com/v3/

Things it does:

* version is not in the URL - it is sent as a header
    ```
    X-GitHub-Media-Type: github.v3
    ```
    You request a version of the api using the Accept header:
    ```
    Accept: application/vnd.github.v3+json
    ```

* very explorable, they seem to have embraced hypermedia apis
* blank fields are included as null not ommited
* all times are in ISO 8601 timestamp format.
    * they also allow setting timezone with the `Time-Zone` header
* error objects contain a `documentation_url` property that links to their docs
* when there are urls that need parameters added they demo them in the URL wrapped in `{}` e.g.
    ```
    members_url: "https://api.github.com/orgs/GritLearning/members{/member}",
    ```
* properties ending in `*_url` are in RFC 6570 URL Template format so you can use a URL templating library to work iwth them
    * http://tools.ietf.org/html/rfc6570


# JSON API

Heroku has a good post on API design https://github.com/interagent/http-api-design
TODO: contrast the heroku post with what JSON API does


* A JSON object is the root of _every_ JSON API response
* Response is either "primary data" or "array of error objects"

Data document structure optional encased in []

* (object)
    * data (object, array, scalar)
    * meta [object]
        * non standard info about the primary data i.e. anything that cannot be represented as an attribute or relationship
    * links [object]
        * URLs related to the primary data
    * included [object]
        * a list of resrouce objects related to the primary one and included in this response

* Primary data must be under "data" key


Error document structure

* (object)
    * errors (array)


* server
    * rails
        * json-api::Resources is aiming to be fully compliant and is under heavy dev to get there
        * activemodel::serializers is not currently as compliant as JSONAPI::Resources but might get there too
* js
    * ember-data
        * currently ember-data is not 100% compatibile but they are aiming to be once JSON API hits 1.0
            * https://github.com/emberjs/data/issues/1988
    * ember-json-api
        * an adaptor for ember-data to make it JSON API compliant
            * unclear what its future will be
