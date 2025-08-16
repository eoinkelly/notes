## Environment variables which control bundler

    BUNDLE_PATH - The location on disk to install gems. Defaults to $GEM_HOME in development and vendor/bundle when --deployment is used.

In "deployment mode" gems are installed to `vendor/bundle` not your "default
system location"

> When you run bundle install, bundler will (by default), install your gems to
> your system repository of gems. This means that they will show up in gem listk
