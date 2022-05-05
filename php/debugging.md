
??? what are my options for getting a php with xdebug?

```
pecl install xdebug # doesn't work on macos 2022
```

## Set up VSCode XDebug integration

Add the chunk below to your php.ini via `phpenv configure` (opens EDITOR with the current php.ini)

```ini
[XDebug]
xdebug.remote_enable = 1
xdebug.remote_autostart = 1

# Now set the memory limit to something big (PHPCS requires a lot of memory)
memory_limit = 1024M
```
