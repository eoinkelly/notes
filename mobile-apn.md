# APN

GGSN

* Gateway GPRS Support Node (GGSN)

APN

* the name of a gateway between a mobile (radio) network (e.g. GSM/3G/4G) and another computer network (often the Internet)
* the APN identifies the packet data network (PDN) that a mobile data user wants to communicate with
* How it appears in phone settings
    * The APN setting on iPhone has APN, username, password
        * Username and password only required if your carrier requires you to supply them to access data
    * Apparently you can also specify an APN type which decides what kind of data should go to that APN but I didn't see any such option on iPhones

https://en.wikipedia.org/wiki/Access_Point_Name

Anatomy of an APN

```
<NETWORK_ID>.mnc<MNC>.mcc<MCC>.gprs
            |<-- operator id ---->|
```

* Network identifier
    * `<NETWORK_ID>` in diagram above
    * required
    * often looks like a domain name associated with the carrier
* operator identifier
    * optional and doesn't seem to be super common in usage
    * `<MNC>`
        * Mobile country code
    * `<MCC>`
        * Mobile network code
    * each carrier has their own unique MCC, MNC pair

LTE has a diff format

    # Old format
    internet.mnc012.mcc345.gprs

    # LTE format (note addition of .apn.epc and replacement of .gprs iwth .3gppnetwork.org
    internet.apn.epc.mnc012.mcc345.3gppnetwork.org



> Your carrier reads these settings in order to generate an IP address, connect
> to the correct secure gateway, and see the carrier needs to connect you to a
> private network like a VPN. All the heavy lifting is done on the carrier
> side, but we need to make sure the right settings are in place to get on the
> network we need, in the way we need to connect.
