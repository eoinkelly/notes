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
