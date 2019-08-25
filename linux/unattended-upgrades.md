# Unattended upgrades

On Ubuntu18 this is controlled by systemd

```systemd
# /lib/systemd/system/apt-daily.timer
[Unit]
Description=Daily apt download activities

[Timer]
OnCalendar=*-*-* 6,18:00
RandomizedDelaySec=12h
Persistent=true

[Install]
WantedBy=timers.target
```

`OnCalendar=*-*-* 6,18:00` parses as _every day at 6am and 6pm_


```systemd
# /lib/systemd/system/apt-daily.service
[Unit]
Description=Daily apt download activities
Documentation=man:apt(8)
ConditionACPower=true
After=network.target network-online.target systemd-networkd.service NetworkManager.service connman.service

[Service]
Type=oneshot
ExecStartPre=-/usr/lib/apt/apt-helper wait-online
ExecStart=/usr/lib/apt/apt.systemd.daily update
```

The shellscript `/usr/lib/apt/apt.systemd.daily` runs `/usr/bin/unattended-upgrade` python script

Configuration files:

* `/etc/apt/apt.conf.d/50unattended-upgrades`
* `/etc/apt/apt.conf.d/20auto-upgrades`

All the configuration in these files configures the shellscript `/usr/lib/apt/apt.systemd.daily`


Note: all daily apt tasks are run by one script.
