

Rails configuration options

    config.logger
        * if set then rails will not
            1. create a logger
            2. set the formatter
        * rails will still set the log level
    config.log_level
        * sets the log level, no fallback, must exist
    config.log_formatter
        * the logger that is created by rails has it's formatter set to this with no fallbacks so it must exist
        * defaults to `ActiveSupport::Logger::SimpleFormatter.new`
            * https://github.com/rails/rails/blob/fbe2433be6e052a1acac63c7faf287c52ed3c5ba/railties/lib/rails/application/configuration.rb#L52
    config.default_log_file
        :: File
        set to `log/ENV_NAME.log` by default

Default rails logger setup:

```ruby
# https://github.com/rails/rails/blob/fbe2433be6e052a1acac63c7faf287c52ed3c5ba/railties/lib/rails/application/bootstrap.rb#L35
logger = ActiveSupport::Logger.new(config.default_log_file)
logger.formatter = config.log_formatter
logger = ActiveSupport::TaggedLogging.new(logger)
```

classes involved

Logger
Logger::Formatter
ActiveSupport::Logger
ActiveSupport::Logger::SimpleFormatter

ActiveSupport::TaggedLogging
    takes an instance of a logger as initialize arg
