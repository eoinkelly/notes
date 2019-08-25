## Idea: Don't load all of Rails

```ruby
# config/application.rb

# Replace this default ...
require 'rails/all'

# ... by individually loading just the parts of Rails you actually need e.g.
require "active_record/railtie"
require "active_storage/engine"
require "action_controller/railtie"
require "action_view/railtie"
require "action_mailer/railtie"
require "active_job/railtie"
require "action_cable/engine"
require "action_mailbox/engine"
require "action_text/engine"
require "rails/test_unit/railtie"
require "sprockets/railtie"
```

* Common targets for omission
    * actionmailer
    * activestorage
    * action_cable
* This will save you a few MB per gem (not super dramtic but can be nice)
* You can used derailed_benchmarks to see how much mem each of those gems is actually using
