# Coordinated Universal Time (UTC)

- does NOT observe daylight savings time
- is interchangeable with GMT except if you are doing hard science (GMT is not
  scientifically defined)
- is around since 1960
- Has had a number of manual adjustments over the years
- Has "leap seconds" to simplify future adjustments
    - leap seconds are added at irregular intervals to compensate for slowing of
      the earths rotation
- A "time zone" is expressed as a positive/negative offset from UTC
    - most time zones are a whole number offset but some regions have 0.5 or
      even 0.25 offsets
- "civil time" = UTC +/- "offset for the current time zone" +/- daylight savings
  adjustment
- NTP (network time protocol) uses UTC exclusively
- Used in aviation, flight-plans etc.
- the time zone that uses UTC (great britian, ireland etc.) is denoted
  `UTC+-00:00` or `Z`
    - causes UTC to be known as "zulu time" from phoenetic alphabet
    -

# ISO8601 representation

    "2015-08-18T13:44:22+12:00"

# Date & time in Postgres

- Rails will always convert time zone to UTC when ever it writes to or reads
  from the database, no matter what time zone you set in the configuration file.
- Rails creates timestamps with the postgres type `TIMESTAMP WITHOUT TIME ZONE`
- => When you look at a timestamp in table of a rails app you are looking at UTC
- PG expresses them as

    2015-07-09 02:59:38.805198 2015-07-09 02:59:38.822467

# Ruby built-in classes

    DateTime < Date < Object
    Date < Object
    Time < Object

- Time
    - only supports UTC and whatever the system time zone is
    - can only represent dates between 1823 and 2116
    - stores integer representing the no. of seconds since the epoch
    ```
    require 'time'
    Time.now
    Time.now.utc
    Time.now.iso8601
    Time.now.utc.iso8601
    ```

    - ActiveSupport decorates it
        - http://api.rubyonrails.org/classes/Time.html
- Date
    - ActiveSupport decorates it
        - http://api.rubyonrails.org/classes/Date.html
- DateTime
    - ActiveSupport decorates it
        - http://api.rubyonrails.org/classes/DateTime.html

# Rails date and time

- ActiveSupport::TimeWithZone
    - designed as a replacement for `Time`
    - a superset of ruby `Time`
    - provides many helpers
    ```
    1.day.ago
    2.weeks.from_now
    Time.now.beginning_of_day
    ```

    - A Time-like class that can represent a time in any time zone. Necessary
      because standard Ruby Time instances are limited to UTC and the system's
      ENV['TZ'] zone.
- TimeZone
    - The TimeZone class serves as a wrapper around TZInfo::Timezone instances

- Rails defaults to using UTC as time zone.
- See all timezones known by rails via `rake time:zones:all`

Setting time zone for a rails app

```
# /config/application.rb

# Set Time.zone default to the specified zone and make Active Record auto-convert to this zone.
# Run "rake -D time" for a list of tasks for finding time zone names. Default is UTC.
config.time_zone = 'Central Time (US & Canada)'
```
