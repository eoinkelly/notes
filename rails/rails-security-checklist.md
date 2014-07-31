# Rails security checklist

## Updated & patched dependencies
* Rails and all gems patched and up to date?
    * how to do this for gems that are not part of rails???

## params
* Have a look at all uses for params[] in the codebase:
Is everything coming from `params` being filtered before being used?
  * any interpolated strings?
  * anywhere it is being used directly e.g. as args to `redirect_to`
Are all methods in controller that are not actions made private?
Does file upload limited to just the filetypes that are not executable?
* Is any sensitive info appearing in the logs?
* CSRF protection present and correct?
* has strong params/mass assignment been set appropriately on all models?
* are all controller actions that change the DB auth checked?
* Is user content always escaped before it is rendered?
* What data is being stored in the Session? - would it be bad if an attacker could see any of it?

## session
* session[] is (by default) stored totally in the cookie so is passed over the network in the clear
* have a look at all uses of session[] in the codebase
    * any unfiltered params going in there?
    * any sensitive info going in there?
* call reset_session after a successful logout ???
* is `config.secret_key_base` appropriately random?

## Passwords
* How are passwords stored in the database?
* Are password policies being enforced?
    * password strength?
    * num failed logins?
    * password expiry/reset?

## SSL

* Is it enabled? Should it be?

# Sources
* http://blog.codeclimate.com/blog/2013/03/27/rails-insecure-defaults/
* https://hakiri.io/facets
* http://guides.rubyonrails.org/security.html
* http://matthewhutchinson.net/2010/10/21/yet-another-rails-security-checklist
