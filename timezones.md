# Timezones

Great article:
https://codeblog.jonskeet.uk/2019/03/27/storing-utc-is-not-a-silver-bullet/

Good info on storing future date-times in a way to manage TZ changes:
https://tools.ietf.org/html/rfc5545#section-3.3.5

## The canonical Timezone database

https://www.iana.org/time-zones

> The Time Zone Database (often called tz or zoneinfo) contains code and data
> that represent the history of local time for many representative locations
> around the globe. It is updated periodically to reflect changes made by
> political bodies to time zone boundaries, UTC offsets, and daylight-saving
> rules.

* IANA releases an updated Timezone database periodically (a few times a year)
* Dataset
    * is approx 500kb zipped
    * a bunch of text files with dates and GMT offsets
    * docs, helper scripts, helper C code
* Programming environments reformat that DB for their own usage and import it

## How operating systems manage TZ data

### Ubuntu

* Ubuntu keeps the TZ data in `/usr/share/zoneinfo/`
* The data is a bunch of binary files in `NZ: timezone data, version 2, 7 gmt time flags, 7 std time flags, no leap seconds, 156 transition times, 7 abbreviation chars` format.
* `tzdata` package. `apt update && apt upgrade tzdata` to get latest version

```
# example on ubuntu when there is a tzdata upgrade available

vagrant@ubuntu-bionic:~$ apt show tzdata -a
Package: tzdata
Version: 2018i-0ubuntu0.18.04
Priority: important
Section: libs
Origin: Ubuntu
Maintainer: Ubuntu Developers <ubuntu-devel-discuss@lists.ubuntu.com>
Original-Maintainer: GNU Libc Maintainers <debian-glibc@lists.debian.org>
Bugs: https://bugs.launchpad.net/ubuntu/+filebug
Installed-Size: 3104 kB
Provides: tzdata-buster
Depends: debconf (>= 0.5) | debconf-2.0
Replaces: libc0.1, libc0.3, libc6, libc6.1
Homepage: https://www.iana.org/time-zones
Task: minimal
Supported: 5y
Download-Size: 189 kB
APT-Manual-Installed: yes
APT-Sources: http://archive.ubuntu.com/ubuntu bionic-updates/main amd64 Packages
Description: time zone and daylight-saving time data
 This package contains data required for the implementation of
 standard local time for many representative locations around the
 globe. It is updated periodically to reflect changes made by
 political bodies to time zone boundaries, UTC offsets, and
 daylight-saving rules.

Package: tzdata
Version: 2018d-1
Priority: important
Section: libs
Origin: Ubuntu
Maintainer: Ubuntu Developers <ubuntu-devel-discuss@lists.ubuntu.com>
Original-Maintainer: GNU Libc Maintainers <debian-glibc@lists.debian.org>
Bugs: https://bugs.launchpad.net/ubuntu/+filebug
Installed-Size: 3088 kB
Provides: tzdata-buster
Depends: debconf (>= 0.5) | debconf-2.0
Replaces: libc0.1, libc0.3, libc6, libc6.1
Homepage: https://www.iana.org/time-zones
Task: minimal
Supported: 5y
Download-Size: 188 kB
APT-Sources: http://archive.ubuntu.com/ubuntu bionic/main amd64 Packages
Description: time zone and daylight-saving time data
 This package contains data required for the implementation of
 standard local time for many representative locations around the
 globe. It is updated periodically to reflect changes made by
 political bodies to time zone boundaries, UTC offsets, and
 daylight-saving rules.
```

### macOS

* Apple will push updated info to your device periodically - see https://support.apple.com/en-us/HT206986
* macOS requires a restart to accept it
* macOS keeps the TZ data in `/usr/share/zoneinfo/`

### Windows

* Presumably Windows does something similar to macOS

## How Elixir manages TZ data

* https://github.com/lau/tzdata
* The package will (at startup) do an automatic download of the new TZ database release when one is available

## How Ruby manages TZ data

Ruby uses the `tzinfo` gem

### tzinfo

* https://github.com/tzinfo/tzinfo

* TZInfo requires a source of time zone data. There are two options:
    1. A zoneinfo directory containing timezone definition files. These files are generated from the [IANA Time Zone Database](https://www.iana.org/time-zones) using the `zic` utility. Most Unix-like systems include a zoneinfo directory.
    2. The TZInfo::Data library (the tzinfo-data gem). TZInfo::Data contains a set of Ruby modules that are also generated from the IANA Time Zone Database.

By default, TZInfo will attempt to use TZInfo::Data. If TZInfo::Data is not
available (i.e. if `require 'tzinfo/data'` fails), then TZInfo will search for a
zoneinfo directory instead (using the search path specified by
`TZInfo::ZoneinfoDataSource::DEFAULT_SEARCH_PATH`).

If no data source can be found, a `TZInfo::DataSourceNotFound` exception will be
raised when TZInfo is used.

```
# on macos
[2] pry(main)> require "tzinfo"
=> true
[3] pry(main)> TZInfo::ZoneinfoDataSource::DEFAULT_SEARCH_PATH
=> ["/usr/share/zoneinfo", "/usr/share/lib/zoneinfo", "/etc/zoneinfo"]
```

### tzinfo-data gem

* https://github.com/tzinfo/tzinfo-data
* contains the tz database for ruby
* releases new versions corresponding to the IANA DB releases
* is automatically used by `tzinfo` if present
* is not installed by Rails by default

### Rails

* Rails requires `tzinfo` via `activesupport` but does not include `tzinfo-data`
* Seems to use the OS tz data by default

## How JS manages TZ data

* There seem to be a few options for TZ aware date calculations in Node
* Example NPM packages for node:
    * https://www.npmjs.com/package/timezonecomplete (a library which uses the data)
    * https://www.npmjs.com/package/tzdata (the actualy tz data)

