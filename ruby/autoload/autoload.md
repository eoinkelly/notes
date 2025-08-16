# Module#autoload

- also available as `Kernel#autoload` (i.e. bare `autoload`)
- lets defer loading a file with `require` until it is actually used by the
  running code
- good for situations where
    - you have **many** ruby files
    - not all of them are necessary for every run of the program
    - parsing them all at program start would slow things down e.g. Rails

### Example

```ruby
# from the devise gem

module Devise
  autoload :Delegator,          'devise/delegator'
  autoload :Encryptor,          'devise/encryptor'
  autoload :FailureApp,         'devise/failure_app'
  autoload :OmniAuth,           'devise/omniauth'
  autoload :ParameterFilter,    'devise/parameter_filter'
  autoload :ParameterSanitizer, 'devise/parameter_sanitizer'
  autoload :TestHelpers,        'devise/test_helpers'
  autoload :TimeInflector,      'devise/time_inflector'
  autoload :TokenGenerator,     'devise/token_generator'
  autoload :SecretKeyFinder,    'devise/secret_key_finder'

  module Controllers
    autoload :Helpers,        'devise/controllers/helpers'
    autoload :Rememberable,   'devise/controllers/rememberable'
    autoload :ScopedViews,    'devise/controllers/scoped_views'
    autoload :SignInOut,      'devise/controllers/sign_in_out'
    autoload :StoreLocation,  'devise/controllers/store_location'
    autoload :UrlHelpers,     'devise/controllers/url_helpers'
  end
  # ...
end

# =============================================================
# is just a lazy version of
# =============================================================

require 'devise/delegator'
require 'devise/encryptor'
require 'devise/failure_app'
require 'devise/omniauth'
require 'devise/parameter_filter'
require 'devise/parameter_sanitizer'
require 'devise/test_helpers'
require 'devise/time_inflector'
require 'devise/token_generator'
require 'devise/secret_key_finder'

require 'devise/controllers/helpers'
require 'devise/controllers/rememberable'
require 'devise/controllers/scoped_views'
require 'devise/controllers/sign_in_out'
require 'devise/controllers/store_location'
require 'devise/controllers/url_helpers'

# you don't actually need this declaration at all for the code in this snippet
# (there is other code in the real devise example which does need the module
# declaration
module Devise
    module Controllers
    end
end
```
