# RSSI

* stands for "received signal strength indicator"
* a value which can be read from the wireless chipset
* a dimensionless number which indicates quality
* spec says number must be between 0 and 255 but manufacturers often choose
  different ranges e.g. 0-60, 0-100 which makes RSSI number not very useful for
  comparing between chipsets.
* Instead of RSSI number, the "received signal power" measured in dBm is often used instead.

# dBm

* a ratio of two powers
* represents "the amount by which this power value is differnt to 1 mW"
* power measured Vs `1mW` (milliwatt)
* ++ can express very large and small power values in a short form
* it can be used as a measurement of absolute power
* for every 10 dBm you drop in power the real power drops 10x

    0       = 1 mW
    -3 dBm  ~= 0.5 mW (2x drop in power)
    -10 dBm ~= 0.1 mW (10x drop in power)
    -20 dBm ~= 0.01 mW (100x drop in power)
    -30 dBm ~= 0.001 mW (1000x drop in power)
    -50 dBm ~= 0.00001 mW (10,000x drop in power)
    -80 dBm ~= 0.00001 mW (10,000,000x drop in power)

## dBm in wifi

* 0 dBm => you are getting 1 mW of power from the transmitter
* -50 dBm => you are getting one ten-thousandth of 1 mW from the transmitter
* levels less than -80db may not be usable, depending on noise.
* Summary:
    * 0 to -50 is great,
    * -50 to -80 goes from great to bad,
    * -100 is the absolute min.
    * The usable min may be much higher than -100 dBm depending on the noise in the environment.

Sometimes wifi quality is mapped onto a percentage value to make it easier for humans to understand.

While there is no simple precise solution that is used universally, we will try
to explain the approximate correlation between received signal power and quality
(percentage).

Generally,

    db >= -50 db  ~= 100% quality
    db <= -100 db ~= 0%   quality

For RSSI signal between -50db and -100db,

    quality ~= 2* (db + 100)
    RSSI ~= (percentage / 2) - 100

For example:

    High quality:       90% ~= -55db
    Medium quality:     50% ~= -75db
    Low quality:        30% ~= -85db
    Unusable quality:   8%  ~= -96db

Sources

* http://www.speedguide.net/faq/how-does-rssi-dbm-relate-to-signal-quality-percent-439
* http://www.metageek.com/training/resources/understanding-rssi.html
