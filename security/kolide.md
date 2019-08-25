

Runs 3 binaries on linux

1. launcher
1. osqueryd
1. osquery-extension.ext
    * communicates with osqueryd via its "extensions socket"

```
$ systemd-cgls

  ├─launcher.kolide-k2.service
  │ ├─11289 /usr/local/kolide-k2/bin/launcher -config /etc/kolide-k2/launcher.flags
  │ ├─11350 /usr/local/kolide-k2/bin/osqueryd --pidfile=/var/kolide-k2/k2device.kolide.com/osquery.pid --database_path=/var/kolide-k2/k2device.kolide.com/osquery.db --extensions_socket=/var/kolide-k2/k2device.kolide.com/osquery.sock --extensions_autoload=/var/kolide-k2/k2device.kolide.com/osquery.autoload --extension
  │ └─11357 /usr/local/kolide-k2/bin/osquery-extension.ext --socket /var/kolide-k2/k2device.kolide.com/osquery.sock --timeout 10 --interval 3
```

Sample launcher flags on linux

```
with_initial_runner
control
autoupdate
root_directory /var/kolide-k2/k2device.kolide.com
osqueryd_path /usr/local/kolide-k2/bin/osqueryd
enroll_secret_path /etc/kolide-k2/secret
control_hostname k2control.kolide.com
update_channel stable
transport jsonrpc
hostname k2device.kolide.com
```


Runs 2 binaries on macOS

```
root             57226   0.0  0.0  4391176    240   ??  S     1:57pm   0:00.04 /usr/local/kolide-k2/bin/osquery-extension.ext --socket /var/kolide-k2/k2device.kolide.com/osquery.sock --timeout 10 --interval 3
root             57225   0.0  0.1  4382116  22816   ??  S     1:57pm   4:36.82 /usr/local/kolide-k2/bin/osqueryd --pidfile=/var/kolide-k2/k2device.kolide.com/osquery.pid
                                                                                                                    --database_path=/var/kolide-k2/k2device.kolide.com/osquery.db
                                                                                                                    --extensions_socket=/var/kolide-k2/k2device.kolide.com/osquery.sock
                                                                                                                    --extensions_autoload=/var/kolide-k2/k2device.kolide.com/osquery.autoload
                                                                                                                    --extensions_timeout=10
                                                                                                                    --config_plugin=kolide_grpc
                                                                                                                    --logger_plugin=kolide_grpc
                                                                                                                    --distributed_plugin=kolide_grpc
                                                                                                                    --disable_distributed=false
                                                                                                                    --distributed_interval=5
                                                                                                                    --pack_delimiter=:
                                                                                                                    --config_refresh=10
                                                                                                                    --host_identifier=uuid
                                                                                                                    --force=true
                                                                                                                    --disable_watchdog
                                                                                                                    --utc
```

