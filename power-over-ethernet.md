# PoE

## Standardised PoE

* PoE standards
    * POE
        * IEEE 802.3af-2003
        * 15.4 W of DC power (12.95 W assumed available at powered device)
        * powered devices are called "type 1" devices
        * autosensing so it will only turn the power on if it detects a PoE device connecting to it.
    * POE+
        * IEEE 802.3at-2009
        * 25.5 W of DC power (??? W assumed available at powered device)
        * powered devices are called "type 2" devices
        * autosensing so it will only turn the power on if it detects a PoE device connecting to it.
    * POE++ or 4PPoE
        * IEEE 802.3bt-2018
        * 60W (51W delivered) (type 3) or 90W (71.3W delivered) (type 4)
        * powered devices are called "type 3" and "type 4" devices
        * autosensing so it will only turn the power on if it detects a PoE device connecting to it.

## Non-standard PoE

### Unifi

Unifi has a thing on older/cheaper devices it calls PoE but it's not standard

* 24 VDC and
* either on or off - no autosensing on ports.

Some Unifi AP-Lites use **only** 24V so you need a device to step down from 48V
e.g. [this Unifi adapter](https://www.amazon.com/Ubiquiti-INS-8023AF-I-802-3AF-Passive-Adapter/dp/B005VSY0KM)

### Cisco UPoE

Cisco has it's own thing called UPoE which provides up to 60W
