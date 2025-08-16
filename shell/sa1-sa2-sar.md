sa1

- is a shell wrapper around sar
- wraper around sadc command
- creates a daily wrapper around sar activities
- `sa1 INTERVAL COUNT`
    - INTERVAL =
- designed to be run from cron

sa2

- a shell wrapper around `sar` `sa2 -A`
- generats a reportin in `/var/log/sa/sarXX` where xx is a number representing
  the day of the month
- designed to be run from cron

sar

- collect/report or save system activity info

sadc

- system activity data collector
- `man sadc` for help
- samples system data by taking COUNT samples every INTERVAL seconds
- writes data into /var/log/sa/saNN where NN is is the day of the month
